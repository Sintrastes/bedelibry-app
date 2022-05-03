{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Montague.Frontend.Route where

import Reflex.Dom.Core hiding (Home)
import Montague.Frontend.NavBar
import qualified Montague.Frontend.Strings as Strings

data Route =
    Schema
  | Home
  | Preferences
  | Entities
  | Types
  | Welcome
    deriving(Eq, Enum, Bounded)

defaultPage = Welcome

pagesWithTabs = 
  [
    Schema
  , Home
  , Preferences
  , Entities
  , Types
  ]

instance Show Route where
    show = \case
        Schema -> show Strings.Schema
        Home   -> show Strings.Home
        Preferences -> show Strings.Preferences
        Entities -> show Strings.Entities
        Types -> show Strings.Types
        Welcome -> "Welcome"

instance DomBuilder t m => HasIcon t m Route where
    icon = \case
        Schema      -> elAttr "i" ("data-feather" =: "file-text") $ pure ()
        Home        -> elAttr "i" ("data-feather" =: "home") $ pure ()
        Preferences -> elAttr "i" ("data-feather" =: "settings") $ pure ()
        Entities    -> elAttr "i" ("data-feather" =: "book") $ pure ()
        Types       -> elAttr "i" ("data-feather" =: "edit") $ pure ()
        Welcome     -> pure ()