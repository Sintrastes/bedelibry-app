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
import Montague.Frontend.Preferences
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

import Montague.Frontend.Home
import Montague.Frontend.Entity
import Montague.Frontend.Types
import Montague.Frontend.Schema

body :: _ => m ()
body = mdo
    topNavEvents <- androidNavBar (stylePref <$> prefs) (enumValues @Page)

    let navEvents = leftmost $ pageNavEvents ++ [topNavEvents, bottomNavEvents]

    (prefs, pageNavEvents) <- tabDisplay defaultPage enumValues navEvents $ do
        maybeParsedSchema <- tab Schema $ schemaPage $ stylePref <$> prefs

        tab Home $ homePage maybeParsedSchema $ stylePref <$> prefs

        prefs <- tab Preferences $ preferencePage (stylePref <$> prefs)

        tab Entities $ entityPage (stylePref <$> prefs) maybeParsedSchema

        tab Types $ typePage (stylePref <$> prefs) maybeParsedSchema

        pure prefs

    currentPage <- holdDyn defaultPage navEvents

    bottomNavEvents <- iOSNavBar currentPage (prefs <&> stylePref) (enumValues @Page)

    prerender (pure ()) $ do
        liftJSM $ eval ("setTimeout(function(){ feather.replace(); }, 50);" :: T.Text)
        pure ()

    prerender (pure never) $ performEvent $ updated (prefs <&> stylePref) <&> \case
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
    pure ()

defaultPage = Schema

data Page =
    Schema
  | Home
  | Preferences
  | Entities
  | Types
    deriving(Eq, Enum, Bounded, Show)

instance DomBuilder t m => HasIcon t m Page where
    icon = \case
        Schema      -> elAttr "i" ("data-feather" =: "file-text") $ pure ()
        Home        -> elAttr "i" ("data-feather" =: "home") $ pure ()
        Preferences -> elAttr "i" ("data-feather" =: "settings") $ pure ()
        Entities    -> elAttr "i" ("data-feather" =: "book") $ pure ()
        Types       -> elAttr "i" ("data-feather" =: "edit") $ pure ()