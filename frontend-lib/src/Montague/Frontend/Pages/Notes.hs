
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Montague.Frontend.Pages.Notes where

import Reflex.Dom.Core hiding (button, tabDisplay)
import qualified Data.Text as T
import System.Info
import Data.Version
import Montague.Frontend.Utils
import Data.Maybe
import qualified Montague.Frontend.Strings as Strings
import Data.Functor

notesPage :: _ => m ()
notesPage = do
    pure ()