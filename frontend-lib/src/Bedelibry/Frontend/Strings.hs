
{-# LANGUAGE LambdaCase #-}

module Bedelibry.Frontend.Strings where

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
  | Small
  | Medium 
  | Large
  | InvalidRuleset
  | RulesetValid 
  | EnterQuery

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
        AppTitle      -> "Bedelibry"
        StylePrefHeader -> "Configure Style"
        StylePrefDescription -> "Specify which overarching style to use with Bedelibry."
        DarkModePrefHeader -> "Enable dark mode"
        DarkModePrefDescription -> "Specify whether or not dark mode is enabled."
        Small  -> "Small"
        Medium -> "Medium"
        Large  -> "Large"
        InvalidRuleset -> "Invalid ruleset (syntax error)"
        RulesetValid -> "Ruleset is valid"
        EnterQuery -> "Enter a query:"