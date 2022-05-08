{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Montague.Frontend.Route where

import Reflex.Dom.Core hiding (Home)
import Montague.Frontend.NavBar
import qualified Montague.Frontend.Strings as Strings
import Montague.Frontend.Utils

data Route =
    Schema
  | Home
  | Preferences
  | Entities { entityAnchor :: Maybe String }
  | Types { typeAnchor :: Maybe String }
  | About
  | Welcome
  | KnowledgeBase
    deriving(Eq)

defaultPage initialPrefs = 
    if dontShowWelcomePage initialPrefs
        then Home
        else Welcome

pagesWithTabs = 
  [
    Home
  , Schema
  , KnowledgeBase
  , Preferences
  , Entities Nothing
  , Types Nothing
  , About
  ]

instance Show Route where
    show = \case
        Schema -> show Strings.Schema
        Home   -> show Strings.Home
        Preferences -> show Strings.Preferences
        Entities _ -> show Strings.Entities
        Types _ -> show Strings.Types
        Welcome -> "Welcome"
        About -> "About"
        KnowledgeBase -> "Knowledge Base"

instance DomBuilder t m => HasIcon t m Route where
    icon = \case
        Schema        -> elAttr "i" ("data-feather" =: "file-text") $ pure ()
        Home          -> elAttr "i" ("data-feather" =: "home") $ pure ()
        Preferences   -> elAttr "i" ("data-feather" =: "settings") $ pure ()
        Entities _    -> elAttr "i" ("data-feather" =: "book") $ pure ()
        Types    _    -> elAttr "i" ("data-feather" =: "edit") $ pure ()
        About         -> elAttr "i" ("data-feather" =: "help-circle") $ pure ()
        KnowledgeBase -> elAttr "i" ("data-feather" =: "book-open") $ pure ()
        Welcome       -> pure ()