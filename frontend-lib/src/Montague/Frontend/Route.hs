{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Montague.Frontend.Route where

import Reflex.Dom.Core hiding (Home)
import qualified Montague.Frontend.Strings as Strings
import Montague.Frontend.Utils

class DomBuilder t m => HasIcon t m e where
    icon :: e -> m ()

data Route =
    Schema
  | Home
  | Notes
  | Preferences
  | Entities { entityAnchor :: Maybe String }
  | Types { typeAnchor :: Maybe String }
  | About
  | Welcome
  | KnowledgeBase
    deriving(Eq)

defaultPage initialPrefs = 
    if dontShowWelcomePage initialPrefs
        then Notes
        else Welcome

pagesWithTabs = 
  [
    Home
  , Schema
  , KnowledgeBase
  , Preferences
  , Entities Nothing
  , Types Nothing
  , Notes
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
        Notes -> "Notes"

instance DomBuilder t m => HasIcon t m Route where
    icon = \case
        Schema        -> elAttr "i" ("data-feather" =: "file-text") blank
        Home          -> elAttr "i" ("data-feather" =: "home") blank
        Preferences   -> elAttr "i" ("data-feather" =: "settings") blank
        Entities _    -> elAttr "i" ("data-feather" =: "book") blank
        Types    _    -> elAttr "i" ("data-feather" =: "edit") blank
        About         -> elAttr "i" ("data-feather" =: "help-circle") blank
        KnowledgeBase -> elAttr "i" ("data-feather" =: "book-open") blank
        Notes         -> elAttr "i" ("data-feather" =: "edit") blank
        Welcome       -> blank