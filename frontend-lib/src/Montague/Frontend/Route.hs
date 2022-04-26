{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Montague.Frontend.Route where

import Reflex.Dom.Core hiding (Home)
import Montague.Frontend.NavBar

data Route =
    Schema
  | Home
  | Preferences
  | Entities
  | Types
    deriving(Eq, Enum, Bounded, Show)

defaultPage = Schema

instance DomBuilder t m => HasIcon t m Route where
    icon = \case
        Schema      -> elAttr "i" ("data-feather" =: "file-text") $ pure ()
        Home        -> elAttr "i" ("data-feather" =: "home") $ pure ()
        Preferences -> elAttr "i" ("data-feather" =: "settings") $ pure ()
        Entities    -> elAttr "i" ("data-feather" =: "book") $ pure ()
        Types       -> elAttr "i" ("data-feather" =: "edit") $ pure ()