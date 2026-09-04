{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  , runFrontend
  , runFrontendReplacing
  , runFrontendWithConfigsAndCurrentRoute
  , renderFrontendHtml
  , removeHTMLConfigs
  , FrontendMode (..)
  , FrontendWidgetT
  , module Obelisk.Frontend.Cookie
  ) where


#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ >= 906
import Control.Monad (when)
import Data.Functor (void)
#endif
#endif

import Prelude hiding ((.))

import Control.Category
import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.Window as DOM
import Language.Javascript.JSaddle (MonadJSM, JSM, jsNull)
import GHCJS.DOM (currentDocument)
import "ghcjs-dom" GHCJS.DOM.Document (getHead)
import GHCJS.DOM.Node (Node, removeChild_)
import GHCJS.DOM.NodeList (IsNodeList, item, getLength)
import GHCJS.DOM.ParentNode (querySelectorAll)
import Obelisk.Frontend.Cookie
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import Obelisk.Configs
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)
import qualified Obelisk.ExecutableConfig.Lookup as Lookup
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie

type ObeliskWidget t route m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadFix (Performable m)
  , PrimMonad m
  , Prerender t m
  , PrebuildAgnostic t route m
  , PrebuildAgnostic t route (Client m)
  , HasConfigs m
  , HasCookies m
  , MonadIO (Performable m)
  )

type PrebuildAgnostic t route m =
  ( SetRoute t route m
  , RouteToUrl route m
  , MonadFix m
  , HasConfigs m
  , HasConfigs (Performable m)
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  , _frontend_body :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  }

baseTag :: forall route t m. ObeliskWidget t route m => RoutedT t route m ()
baseTag =
  elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes

removeHTMLConfigs :: JSM ()
removeHTMLConfigs = void $ runMaybeT $ do
  doc <- MaybeT currentDocument
  hd <- MaybeT $ getHead doc
  es <- nodeListNodes =<< querySelectorAll hd ("[data-obelisk-executable-config-inject-key]" :: Text)
  for_ es $ removeChild_ hd

-- | Collect all nodes in the node list.
--
-- TODO: this and the version in exe-config/ghcjs/lookup should be
-- upstreamed to jsaddle.
nodeListNodes :: (IsNodeList l, MonadJSM m) => l -> m [Node]
nodeListNodes es = do
  len <- getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (item es) $ if len == 0 then [] else [0..len-1]
  pure $ catMaybes nodes

setInitialRoute :: Bool -> JSM ()
setInitialRoute useHash = do
  window <- DOM.currentWindowUnchecked
  initialLocation <- DOM.getLocation window
  initialUri <- getLocationUri initialLocation
  history <- DOM.getHistory window
  DOM.replaceState history jsNull ("" :: Text) $ Just $
    show $ setAdaptedUriPath useHash "/" initialUri

data FrontendMode = FrontendMode
  { _frontendMode_hydrate :: Bool
    -- ^ There is already a rendering of the DOM in place; hydrate it rather
    -- than building new DOM
  , _frontendMode_adjustRoute :: Bool
    -- ^ The page can't use regular routes, so encode routes into the hash
    -- instead
  }

-- | Is this a build that runs inside a browser, and so hydrates markup the
-- server rendered?
isBrowserBuild :: Bool
isBrowserBuild =
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
  True
#else
  False
#endif

-- | What a previous run of 'runFrontendReplacing' in this page left behind.
--
-- A browser page normally runs the frontend exactly once and is then discarded.
-- A wasm GHCi session breaks that: the page stays put while every @:r@ runs
-- @main@ again. Without this, each reload would stack another live network on
-- top of the last.
data PreviousRun = PreviousRun
  { _previousRun_app :: RunningApp
  , -- | The configs the first run read out of the document.
    --
    -- These have to be carried forward because the document only holds them
    -- once: the server injects them into @\<head\>@, 'removeHTMLConfigs' takes
    -- them straight back out, and a re-run replaces the head wholesale anyway.
    -- Caching them here is what keeps 'Obelisk.Configs.getConfig' working after
    -- a reload without changing what a production page contains.
    _previousRun_configs :: Map Text ByteString
  }

-- | Mutable global state, which wants justifying.
--
-- It cannot be a parameter or a return value. A reload is @:r@ followed by a
-- fresh evaluation of the entry point, and those are two unrelated top-level
-- @IO@ actions: GHCi discards the previous statement's bindings, so there is
-- nothing to thread a handle through. The only place an answer to "what did the
-- last run leave behind?" can live is somewhere that outlives @:r@.
--
-- Which is why it is a CAF /in this library/ rather than in the application: a
-- @:r@ resets the top-level state of the modules it reloads, but not that of
-- their dependencies. The same definition written in the app would be wiped by
-- the very reload it exists to survive.
{-# NOINLINE previousRun #-}
previousRun :: IORef (Maybe PreviousRun)
previousRun = unsafePerformIO $ newIORef Nothing

-- | Run the frontend, setting the initial route to "/" on platforms where no
-- route exists ambiently in the context (e.g. anything but web).
-- Selects FrontendMode based on platform; this doesn't work for jsaddle-warp
--
-- Assumes the page has not run the frontend before; see 'runFrontendReplacing'
-- for the interactive case, where it has.
runFrontend
  :: forall backendRoute route
  .  Encoder Identity Identity (R (FullRoute backendRoute route)) PageName
  -> Frontend (R route)
  -> JSM ()
runFrontend validFullEncoder frontend = do
  let mode = FrontendMode
        { _frontendMode_hydrate =
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
          True
#else
          False
#endif
        , _frontendMode_adjustRoute =
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
          False
#else
          True
#endif
        }
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
  configs <- Lookup.getConfigs
#else
  configs <- liftIO Lookup.getConfigs
#endif
  when (_frontendMode_hydrate mode) removeHTMLConfigs
  -- There's no fundamental reason that adjustRoute needs to control setting the
  -- initial route and *also* the useHash parameter; that's why these are
  -- separate here.  However, currently, they are always the same.
  when (_frontendMode_adjustRoute mode) $ do
    setInitialRoute $ _frontendMode_adjustRoute mode
  void $ runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend

-- | 'runFrontend', for a page that may already be running the app.
--
-- This is what an interactive session needs. A wasm GHCi session lives inside
-- the page, so the page is never reloaded: every @:r@ evaluates the entry point
-- again against a document that already has a live app. Left alone that stacks a
-- second network on the first, tries to hydrate markup the previous run already
-- replaced, and finds no configs at all -- 'removeHTMLConfigs' took those out of
-- the document during the first run.
--
-- Reloading the page instead is not an option, which is what makes all this
-- necessary: the interpreter's state /is/ the wasm heap in that tab -- loaded
-- object code, CAFs and all -- so navigating away destroys the session that is
-- driving the reload.
--
-- So the previous run is retired, the configs it read are reused, and hydration
-- is skipped. Skipping hydration is also what disposes of the old DOM, for free:
-- 'runImmediateWidgetWithHeadAndBody' replaces the head and body contents rather
-- than appending to them.
--
-- The first call in a page behaves exactly like 'runFrontend'. Only interactive
-- sessions should call this -- obelisk's @ob@ submits it in place of the entry
-- point -- and a compiled frontend keeps using 'runFrontend'.

--   [Leaked timer threads] Re-running the frontend retires the previous run's
--   network (see 'Obelisk.Frontend.runFrontendReplacing'), but threads that
--   @performEventAsync@ and friends forked outlive it. They write to a channel
--   nobody reads, so they are inert, but they are not collected either.

-- @Main.reloadMain@, not @Main.main@: the tab is never reloaded (the GHCi session
-- lives in it), so every load re-runs the app in a page that already has one.
-- 'Obelisk.Frontend.runFrontendReplacing' retires the previous run and rebuilds
-- instead of hydrating a second time, which also means nothing here has to clear
-- the DOM.

runFrontendReplacing
  :: forall backendRoute route
  .  Encoder Identity Identity (R (FullRoute backendRoute route)) PageName
  -> Frontend (R route)
  -> JSM ()
runFrontendReplacing validFullEncoder frontend = do
  -- One page, one application -- true of a browser build, where the frontend
  -- runs inside the page. A jsaddle-warp build instead hands each browser
  -- connection its own run of this same action, so there "the previous run" is
  -- someone else's tab; fall back to 'runFrontend' behaviour.
  previous <- if isBrowserBuild then liftIO (readIORef previousRun) else pure Nothing
  let mode = FrontendMode
        { _frontendMode_hydrate = isBrowserBuild && isNothing previous
        , _frontendMode_adjustRoute = not isBrowserBuild
        }
  configs <- case previous of
    Just p -> pure $ _previousRun_configs p
    Nothing -> do
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
      cs <- Lookup.getConfigs
#else
      cs <- liftIO Lookup.getConfigs
#endif
      when (_frontendMode_hydrate mode) removeHTMLConfigs
      pure cs
  -- Before building the new one, so the old network cannot react to anything
  -- while the new DOM is going in.
  for_ previous $ liftIO . stopApp . _previousRun_app
  when (_frontendMode_adjustRoute mode) $ do
    setInitialRoute $ _frontendMode_adjustRoute mode
  app <- runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend
  when isBrowserBuild $ liftIO $ writeIORef previousRun $ Just $ PreviousRun app configs

runFrontendWithConfigsAndCurrentRoute
  :: forall backendRoute frontendRoute
  .  FrontendMode
  -> Map Text ByteString
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName
  -> Frontend (R frontendRoute)
  -> JSM RunningApp
runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend = do
  let ve = validFullEncoder . hoistParse errorLeft (reviewEncoder (rPrism $ _FullRoute_Frontend . _ObeliskRoute_App))
      errorLeft = \case
        Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
        Right x -> Identity x
      w :: ( RawDocument (DomBuilderSpace (HydrationDomBuilderT s DomTimeline m)) ~ DOM.Document
           , Ref (Performable m) ~ Ref IO
           , Ref m ~ Ref IO
           , DomBuilder DomTimeline (HydrationDomBuilderT s DomTimeline m)
           , MonadHold DomTimeline m
           , MonadRef m
           , MonadRef (Performable m)
           , MonadReflexCreateTrigger DomTimeline m
           , PerformEvent DomTimeline m
           , PostBuild DomTimeline m
           , PrimMonad m
           , MonadSample DomTimeline (Performable m)
           , DOM.MonadJSM m
           , MonadFix (Performable m)
           , MonadFix m
           , Prerender DomTimeline (HydrationDomBuilderT s DomTimeline m)
           , MonadIO (Performable m)
           )
        => (forall c. HydrationDomBuilderT s DomTimeline m c -> FloatingWidget () c)
        -> (forall c. HydrationDomBuilderT s DomTimeline m c -> FloatingWidget () c)
        -> FloatingWidget () ()
      w appendHead appendBody = do
        rec switchover <- runRouteViewT ve switchover (_frontendMode_adjustRoute mode) $ do
              (switchover'', fire) <- newTriggerEvent
              mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendHead . runConfigsT configs))) $ do
                -- The order here is important - baseTag has to be before headWidget!
                baseTag
                _frontend_head frontend
              mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendBody . runConfigsT configs))) $ do
                _frontend_body frontend
                switchover' <- case _frontendMode_hydrate mode of
                  True -> lift $ lift $ lift $ lift $ HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
                  False -> getPostBuild
                performEvent_ $ liftIO (fire ()) <$ switchover'
              pure switchover''
        pure ()
  if _frontendMode_hydrate mode
    then runHydrationWidgetWithHeadAndBody (pure ()) w
    else runImmediateWidgetWithHeadAndBody w

type FrontendWidgetT r = RoutedT DomTimeline r (SetRouteT DomTimeline r (RouteToUrlT r (ConfigsT (CookiesT (HydratableT (PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))))))))

renderFrontendHtml
  :: MonadIO m
  => Map Text ByteString
  -> Cookies
  -> (r -> Text)
  -> r
  -> Frontend r
  -> FrontendWidgetT r ()
  -> FrontendWidgetT r ()
  -> m ByteString
renderFrontendHtml configs cookies urlEnc route frontend headExtra bodyExtra = do
  --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  html <- fmap snd $ liftIO $ renderStatic $ runHydratableT $ fmap fst $ runCookiesT cookies $ runConfigsT configs $ flip runRouteToUrlT urlEnc $ runSetRouteT $ flip runRoutedT (pure route) $
    -- The lang attribute only needs to exist server-side: hydration appends
    -- into the existing head/body and never replaces the html element.
    elAttr "html" ("lang" =: "en") $ do
      el "head" $ do
        baseTag
        injectExecutableConfigs configs
        _frontend_head frontend
        headExtra
      el "body" $ do
        _frontend_body frontend
        bodyExtra
  return $ "<!DOCTYPE html>" <> html
