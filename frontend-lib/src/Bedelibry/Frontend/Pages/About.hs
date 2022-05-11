{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Bedelibry.Frontend.Pages.About where

import Reflex.Dom.Core hiding (button, tabDisplay)
import qualified Data.Text as T
import System.Info
import Data.Version
import Bedelibry.Frontend.Utils

aboutPage :: _ => m ()
aboutPage = noScrollPage $ do
    el "p" $
        text $ "Bedelibry is an app that lets the user specify a schema, and use that schema to parse" <>
            " natural language expressions according to that schema."
    el "p" $
        text $ "Built with " <> T.pack compilerName <> " " <> 
            T.pack (showVersion compilerVersion) <> " for " <>
            T.pack arch <> " " <> T.pack os <> "."