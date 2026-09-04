{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- An explicit export list because a module called Main otherwise exports `main`
-- alone, and `ob` needs to reach `reloadMain` by name from the repl.
module Main (main, reloadMain) where

import Obelisk.Frontend
import Reflex.Dom

import Common.Route

import Frontend



main :: IO ()
main = run $ runFrontend checkedFullRouteEncoder frontend

-- | Entry point for an interactive session, where the same page runs the app
-- again on every reload: replaces the previous run rather than stacking on it.
-- Obelisk's @ob@ submits this in place of 'main'.
reloadMain :: IO ()
reloadMain = run $ runFrontendReplacing checkedFullRouteEncoder frontend

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif
