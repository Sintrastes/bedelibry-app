
{-# LANGUAGE LambdaCase #-}

module Montague.Frontend.Strings where

data Strings =
    Home
  | Entities
  | Preferences
  | Schema
  | Types
  | EnterSentence
  | EnterSchema
  | SchemaValid
  | InvalidSchema
  | CouldNotParse
  | Save
  | NewEntity
  | NewType
  | Name
  | Description
  | AppTitle
  | StylePrefHeader
  | StylePrefDescription
  | DarkModePrefHeader
  | DarkModePrefDescription

instance Show Strings where
    show = \case
        Home          -> "Home"
        Entities      -> "Entities"
        Preferences   -> "Preferences"
        Schema        -> "Schema"
        Types         -> "Types"
        SchemaValid   -> "Schema valid"
        Save          -> "Save"
        InvalidSchema -> "Invalid Schema"
        EnterSchema   -> "Enter in the schema for your data"
        EnterSentence -> "Enter in a sentence you want to parse"
        CouldNotParse -> "Could not parse"
        NewEntity     -> "New Entity"
        NewType       -> "New Type"
        Name          -> "Name"
        Description   -> "Description"
        AppTitle      -> "Montague"
        StylePrefHeader -> "Use Android style"
        StylePrefDescription -> "Specify whether or not to use the Android theme."
        DarkModePrefHeader -> "Enable dark mode"
        DarkModePrefDescription -> "Speicfy whether or not darn mode is enabled."