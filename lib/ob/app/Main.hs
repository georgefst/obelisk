{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable
import Data.Functor
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Zipper
import Graphics.Vty qualified as Vty
import Network.Socket (PortNumber)
import Options.Applicative qualified as OA
import Reflex.Network
import Reflex.Process
import Reflex.Process.GHCi
import Reflex.Vty
import System.Environment
import System.FilePath
import System.Info qualified as Info
import System.Posix.Signals qualified as Sig
import System.Process
import Web.Browser

-- TODO move to separate module?
-- should probably be one for the TUI (with its helpers in separate modules again) then a thin CLI layer
-- bear in mind we want to expand this to cover other `ob` commands
-- also, we can then more easily selectively enable ApplicativeDo and RecordWildCards
-- when we do that, we should also move this whole package to a new folder, not in `lib`
data Opts = Opts
  { debug :: Bool
  }
cliParser :: OA.ParserInfo Opts
cliParser =
  OA.info (parser OA.<**> OA.helper) $
    OA.fullDesc
      -- TODO this will ultimately be a subcommand
      <> OA.progDesc "Run Obelisk frontend and backend through REPL with updates on file changes."
 where
  parser =
    Opts
      <$> OA.switch
        ( OA.long "debug"
            <> OA.help "Show raw GHCI output"
        )

-- TODO add to CLI, and maybe just default to picking arbitrary free ones
-- and catch when they're not available?
backendPort :: PortNumber
backendPort = 8000
ghciBrowserPort :: PortNumber
ghciBrowserPort = 8001

-- printed to backend's stdout to indicate server is ready
-- this is arbitrary - all that matters is that we pass this same string for writer and reader
-- analogous to how we watch for the frontend's existing marker
-- TODO probably a better way...
backendReadyMarker :: Text
backendReadyMarker = "obelisk: backend ready"

main :: IO ()
main = do
  opts <- OA.execParser cliParser
  runTUI opts

-- TODO remove anything we don't actually want that's left from https://github.com/reflex-frp/reflex-ghci/blob/develop/src-bin/ghci.hs
-- TODO use non-default theme? `localTheme (const (pure def{})) do`
runTUI :: Opts -> IO ()
runTUI opts = mainWidget def $ initManager_ do
  tabNavigation
  userExitRequested <- void <$> keyCombo (Vty.KChar 'c', [Vty.MCtrl])
  -- `env` in `CreateProcess` _replaces_ the child's environment rather than extending it,
  -- so our own has to be threaded through explicitly or the child loses `PATH`, `NODE_PATH` etc.
  -- TODO this is a bad API, and could be more explicit in the docs
  baseEnv <- liftIO getEnvironment

  -- TODO move this in to a function with a record return type?
  ((frontendBoot, frontendExit, frontendPid), (backendBoot, backendExit, backendPid)) <- do
    let
      renderRepl title ghciProc onLoad readyCmd = tile flex do
        Repl raw finished started exited <- do
          ghciWatchIgnoring
            ( -- TODO replace this slop with something principled
              -- tbh, whitelisting might be better than blacklisting
              -- i.e. just specify the source paths, which ideally we can get from GHC
              -- also, reload on asset changes
              -- (we might want to do something clever to patch those in without a reload - see `css reload slop` stash)
              \dir path ->
                "static/generated" `isInfixOf` path
                  || any
                    (\c -> c == "dist" || "dist-" `isPrefixOf` c || ("." `isPrefixOf` c && c /= "." && c /= ".."))
                    (splitDirectories $ makeRelative dir path)
            )
            -- TODO this is apparently to enable our `sigKILL` to work properly
            -- we should investigate and, if all seems reasonable, at least make the connection more explicit
            ghciProc{create_group = True}
            (Just onLoad)
            never
            userExitRequested
        sessionGroup <- liftIO $ getPid $ _process_handle raw

        -- TODO this seems inefficient
        -- list handling, leaks etc.
        -- and that we use `networkView` on this despite only the last row ever needing to be replaced
        finishedList <- foldDyn (flip (<>)) [] $ Map.toAscList <$> finished
        let allCommands = do
              old <- map (\(_, c) -> (,c) if hasErrors c then Vty.brightRed else Vty.brightGreen) <$> finishedList
              -- TODO note that `hasErrors` uses an imperfect heuristic - worth some serious testing
              -- let errors = maybe False (hasErrors . snd) <$> command
              new <- maybeToList . (\(_, c) -> fmap (Vty.brightYellow,) c) <$> started
              pure $ old <> new

        -- TODO can we avoid accumulating this when --debug is not passed?
        -- Claude says "it is the only record of a session's output before its first command completes"
        -- ah, right, yeah, we use it for now, for detecting whether the session is ready
        -- although tbh for that maybe we should stick to the stdout+stderr reads we already have elsewhere
        -- also, keeping everything is technically a memory leak...
        -- Claude wanted to use use a ring buffer, but tbh not a priority - GHCI isn't _that_ verbose
        rawLog <-
          foldDyn (flip (<>)) ""
            . fmap T.concat
            . mergeWith (<>)
            $ fmap (pure @[] . T.decodeUtf8Lenient) <$> [_process_stdout raw, _process_stderr raw]

        booted <- firstTrue $ fmap (readyCmd . T.strip) rawLog

        tile flex
          . boxTitle (pure TextAlignment_Left) (pure def) (pure title)
          . void
          . scrollable
            -- auto-scrolled to the bottom, so we follows new commands
            def
              { _scrollableConfig_startingPosition = ScrollPos_Bottom
              , _scrollableConfig_scrollToBottom = pure $ Just ScrollToBottom_Maintain
              }
          . fmap (never,)
          . void
          . networkView
          $ allCommands <&> \cmds -> do
            -- `flex` can't work here: `axis` divides its main-axis extent between
            -- its children _before_ they run, so `flex` means "an equal share of
            -- the parent" rather than "as tall as my content", and inside
            -- `scrollable` that parent is a deliberately-huge 10000 rows
            -- (`Reflex.Vty.Widget.Scroll.largeScrollHeight`) - hence a screenful
            -- of blank lines per command, and a scroll bar whose thumb is one row
            -- because `_scrollable_totalLines` is then ~10000 whatever the content.
            -- Sizing to content can't go through the constraint solver at all:
            -- heights are only knowable from the rendered images, i.e. as
            -- `Behavior`s, while the layout tree is `Dynamic`. The upstream fix is
            -- a content-sized (measure-then-stack) container - there's a prototype
            -- (`flowV`) in the reflex-vty checkout.
            -- We don't need it here, though: by the time we get here the log is
            -- pure data, so we can just measure it ourselves and hand the solver a
            -- fixed height. `grout` rather than `tile` because these rows aren't
            -- focusable - with `tile`, Tab cycles through three entries per command.
            -- TODO the next step, if this ever gets slow, is to stop asking the
            -- layout system for anything at all: since we're measuring the content
            -- anyway, we can render the whole log to one image - a `vertCat` of
            -- `horizCat`s of `wrapTextImage` - and `tellImages` it straight off a
            -- `Behavior`. That drops the `networkView` (which currently rebuilds
            -- every row's layout node on every new command, when only the last row
            -- ever changes) and the duplicated wrapping (`commandHeight` wraps the
            -- text to measure it and then `richText` wraps it again to draw it).
            -- Output is identical, including scroll-bar sizing; it's ~12x faster
            -- at a few thousand commands, but that's not a size we hit yet, and
            -- this version stays in the layout system's idiom.
            dw <- displayWidth
            for_ cmds \(color, cmd) ->
              grout (fixed $ flip commandHeight cmd <$> dw)
                . row
                $ for_ (commandColumns cmd) \t ->
                  grout flex
                    . richText
                      (RichTextConfig $ pure $ Vty.defAttr `Vty.withForeColor` color)
                    $ pure t
        -- TODO check how this actually renders once I've finished all the grid layout changes
        when opts.debug
          . void
          . tile flex
          $ scrollableText def rawLog
        pure (booted, exited, sessionGroup)
    backend <-
      let expr =
            Char8.pack $
              unwords
                -- TODO somehow do something cleaner than string concatenation?
                -- at least minimise this by using `-XOverloadedStrings` and adding more specific function to backend
                [ "Backend.runWith "
                , "(Obelisk.Backend.ghciBackendConfig"
                , show backendPort
                , "(Data.Text.pack " <> show (localhost ghciBrowserPort) <> ")"
                , "(Data.Text.pack " <> show backendReadyMarker <> ")"
                , ")"
                ]
       in renderRepl
            "Backend"
            ( proc
                "cabal"
                [ "repl"
                , "-fnative" -- as used by ob-repl etc. to stop `Setup.hs` from cross-compiling the frontend on every load
                , "-frepl"
                , "--enable-multi-repl"
                , "common"
                , "frontend"
                , "lib:backend"
                ]
            )
            (unsafeCommand expr)
            -- TODO huh, why doesn't this work?
            -- or the commented-out one for frontend either
            -- I assumed it was a trailing newline or something, which was why I added `T.strip`
            -- oh, it might be that stdout stuff arrives simultaneously with stderr and gets concat-ed
            -- (== backendReadyMarker)
            (backendReadyMarker `T.isInfixOf`)
    frontend <-
      renderRepl
        "Frontend"
        ( ( proc
              "ob-wasm"
              [ "cabal"
              , "repl"
              , -- CPPs out a foreign export, which would confuse browser mode before a recent patch
                -- TODO get GHC version, and gate this to `< 10` (assuming relevant PR now merged)
                "-fghci-old"
              , "-frepl"
              , -- TODO what about interaction with custom setup? https://github.com/haskell/cabal/issues/10602
                -- and on that note, what exactly is our custom setup doing and when does it re-run?
                "--enable-multi-repl"
              , "common"
              , "frontend"
              ]
          )
            { env =
                Just $
                  baseEnv
                    -- note that passing these to `cabal` via `--repl-options` doesn't work with `--enable-multi-repl`
                    -- TODO report that bug, or add a link to GitLab issue here if there already is one
                    <> [ ("GHCI_BROWSER", "1")
                       , ("GHCI_BROWSER_REDIRECT_WASI_CONSOLE", "1")
                       , ("GHCI_BROWSER_PORT", show ghciBrowserPort)
                       ]
            }
        )
        (unsafeCommand $ Char8.pack "Main.reloadMain")
        -- TODO I have an umerged GHC patch that allows setting a hook for this
        -- in the long run, that would be nicer than string matching on the output
        -- (\t -> "Open" `T.isPrefixOf` t && "to boot ghci" `T.isSuffixOf` t)
        (\t -> ("127.0.0.1:" <> T.show ghciBrowserPort) `T.isInfixOf` t)
    pure (frontend, backend)

  -- TODO we should grey everything out when backend has loaded but frontend `dyld.js` isn't there yet
  -- otherwise we can get some pretty weird behaviour, where the frontend is never properly initialised
  -- we should also maybe link to the GHC manual about about how the user shouldn't refresh the page
  ready <- firstTrue =<< (zipDynWith (&&) <$> hasOccurred backendBoot <*> hasOccurred frontendBoot)
  -- TODO add --no-open flag? some people tend not to want auto-opening
  performEvent_ $ liftIO (void $ forkIO $ void $ openBrowser $ T.unpack $ localhost backendPort) <$ ready
  replsExited <- firstTrue =<< (zipDynWith (&&) <$> hasOccurred frontendExit <*> hasOccurred backendExit)
  -- TODO 10s delay? I don't think we ever even need it, i.e. I've always seen clean exits
  -- actually, much better UX would be to indicate that it's been pressed once, and allow pressing again to force
  -- giveUp <- delay 10 userExitRequested
  -- let done = leftmost [replsExited, giveUp]

  -- handles the Wasm cross GHC, NodeJS etc.
  -- TODO do we actually need this? and if so, should it be pushed upstream?
  -- Claude already seems to think `reflex-ghci` sends `:q` and then terminates the child,
  -- but that somehow `terminateProcess` leaves survivors which keep `GHCI_BROWSER_PORT` bound,
  -- but that "usually the group has already gone", which is why have the no-op `handle @IOException`,
  -- and then there is nothing to signal
  performEvent_ $
    replsExited <&> \() ->
      mapM_ (liftIO . handle @IOException mempty . Sig.signalProcessGroup Sig.sigKILL) $
        catMaybes [backendPid, frontendPid]

  -- TODO what's this for? `reflex-vty` docs don't make it clear what's done with the returned event
  -- presumably, first time this fires, the VTY app quits?
  pure replsExited

-- TODO things to maybe move out to utils module, or check whether they already exist somewhere
-- or if there's a better way, or better name...
-- EDIT: aha `holdUniqDyn` is what I was looking for earlier with the `Eq` constraint
localhost :: PortNumber -> Text
localhost port = "http://localhost:" <> T.show port
hasOccurred :: (Reflex t, MonadHold t m) => Event t a -> m (Dynamic t Bool)
hasOccurred = holdDyn False . (True <$) <=< headE
firstTrue :: (Reflex t, MonadHold t m) => Dynamic t Bool -> m (Event t ())
firstTrue = headE . void . ffilter id . updated

-- | The three columns of a command row: what was sent, and what came back.
commandColumns :: Cmd -> [Text]
commandColumns cmd =
  map
    (T.decodeUtf8Lenient . ($ cmd))
    [displayCommand . _cmd_stdin, unLines . _cmd_stdout, unLines . _cmd_stderr]

{- | How many rows a command will actually draw at total width `w`.
`wrapTextImage` is the very function `richText` uses, so this can't drift out
of agreement with what gets rendered.
-}
commandHeight :: Int -> Cmd -> Int
commandHeight w =
  maximum
    . (1 :)
    . zipWith
      (\colWidth -> Vty.imageHeight . wrapTextImage TextAlignment_Left colWidth Vty.defAttr)
      (evenWidths 3 w)
    . commandColumns

{- | Split an extent between `n` columns the way `axis` splits it between `flex`
children: `w `div` n` each, with the first `w `mod` n` getting one extra.
-}
evenWidths :: Int -> Int -> [Int]
evenWidths n w = [q + if i < r then 1 else 0 | i <- [0 .. n - 1]]
 where
  (q, r) = w `divMod` n
