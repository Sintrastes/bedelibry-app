{-# LANGUAGE LambdaCase
    , MultiParamTypeClasses
    , FunctionalDependencies
    , ScopedTypeVariables
    , TypeApplications
    , DataKinds
    , FlexibleInstances
    , FlexibleContexts
    , RecursiveDo
    , BlockArguments
    , OverloadedStrings
    , ConstraintKinds
    , GADTs
    , PartialTypeSignatures
    , ImplicitParams
    , TemplateHaskell
    , QuasiQuotes #-}

module Montague.Frontend where

import Montague.Frontend.Utils
import Montague.Frontend.Utils.Javascript
import Montague.Frontend.TabDisplay
import Montague.Frontend.Pages.Preferences
import Montague.Frontend.NavBar

import System.Info
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Reflex.Dom.Core hiding (button, tabDisplay, Home)
import Montague
import Data.Maybe
import Data.Function
import Data.Functor
import Prelude hiding ((<=), div)
import Control.Applicative
import Control.Monad.Tree
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Data.Proxy
import Montague.Semantics
import Data.PartialOrd hiding ((==))
import System.Environment
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Fix
import System.Directory
import Data.List
import Control.Monad.Tree

import Montague.Frontend.Route (Route)
import qualified Montague.Frontend.Route as Route

import Montague.Frontend.Pages.Welcome
import Montague.Frontend.Pages.Home
import Montague.Frontend.Pages.Entity
import Montague.Frontend.Pages.Types
import Montague.Frontend.Pages.Schema

body :: _ => m ()
body = mdo
    -- Setup the application directory.
    montagueDir <- if "android" `isInfixOf` os
        then pure "/data/data/org.bedelibry.demos.montague"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.montague")

    -- toastOnErrors $ liftFrontend (Right ()) $ catch
    --     (do createDirectoryIfMissing True montagueDir
    --         pure $ Right ())
    --     (\(e :: SomeException) -> pure $ Left e)

    topNavEvents <- androidNavBar prefs Route.pagesWithTabs

    let navEvents = leftmost $ pageNavEvents ++ [topNavEvents, bottomNavEvents]

    (prefs, pageNavEvents) <- tabDisplay Route.defaultPage navEvents $ do
        maybeParsedSchema <- tab Route.Schema $ schemaPage prefs
            montagueDir

        tab Route.Home $ homePage maybeParsedSchema prefs

        prefs <- tab Route.Preferences $ preferencePage prefs
            montagueDir

        tab Route.Entities $ entityPage prefs maybeParsedSchema

        tab Route.Types $ typePage prefs maybeParsedSchema

        tab Route.Welcome welcomePage

        pure prefs

    currentPage <- holdDyn Route.defaultPage navEvents

    bottomNavEvents <- iOSNavBar currentPage prefs Route.pagesWithTabs

    prerender (pure ()) $ do
        liftJSM $ eval ("setTimeout(function(){ feather.replace(); }, 50);" :: T.Text)
        pure ()

    initialPref <- sample $ current $ prefs <&> stylePref

    prerender (pure ()) $
        initialPref & updateCSS

    prerender (pure never) $ performEvent $ updated (prefs <&> stylePref) <&> 
        updateCSS

    pure ()

updateCSS :: _ => Style -> m ()
updateCSS = \case
    IOS -> do
        modifyLink "css-style"
            "https://sintrastes.github.io/demos/montague/puppertino/newfull.css"
        liftJSM $ eval ("setTimeout(function(){ feather.replace(); }, 50);" :: T.Text)
        pure ()
    Android -> do
        modifyLink "css-style"
            "https://sintrastes.github.io/demos/montague/materialize.min.css"
        liftJSM $ eval ("setTimeout(function(){ feather.replace(); }, 50);" :: T.Text)
        pure ()