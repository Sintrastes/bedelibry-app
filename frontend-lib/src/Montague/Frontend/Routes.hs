
module Montague.Frontend.Routes where

data Route =
    Schema
  | Home
  | Preferences
  | Entities
  | Types
    deriving(Eq, Enum, Bounded, Show)