
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
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Functor

notesPage :: _ => m ()
notesPage = elAttr "div" ("class" =: "container" <> "style" =: "height: 91.5vh;") $ do
    enteredText <- elClass "div" "container__left" $ do
        _textAreaElement_value <$> textAreaElement (
            def & textAreaElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ attrs)
    elAttr "div" ("class" =: "resizer" <> "id" =: "dragMe") blank
    elClass "div" "container__right" $
        dynText enteredText
    
    prerender (pure ()) $ do
        liftJSM $ eval ("setTimeout(function(){ addPaneDragListener(); }, 50)" :: T.Text)
        pure ()

    pure ()
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "text" <> "style" =: "height: 100%; width: 100%;"