{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Montague.Frontend.Pages.KnowledgeBase where

import Reflex.Dom.Core hiding (button, tabDisplay)
import qualified Data.Text as T
import System.Info
import Data.Version
import Montague.Frontend.Utils
import Data.Maybe

import Ast
import Parser

knowledgeBasePage :: DomBuilder t m => m (Dynamic t (Maybe Program))
knowledgeBasePage = noScrollPage $ do
    let textAttrs = "spellcheck" =: "false" <>
          "class" =: "input-field col s12" <> 
          "style" =: "display:flex;height:75%;width:100%;"

    el "p" $ text "Enter in the facts for your data"

    schemaText <- elAttr "div" textAttrs $ textAreaElement (
        def & textAreaElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "p-form-text p-form-no-validate" <>
                 "style" =: ("resize: none;height: 100%;width:100%;" <> boxResizing))
            & textAreaElementConfig_initialValue
            .~ T.pack ""
     )

    pure $ eitherToMaybe . clausesFromString "" . T.unpack <$> 
        _textAreaElement_value schemaText
  where boxResizing = "-webkit-box-sizing: border-box;"
          <> "-moz-box-sizing: border-box;"
          <> "box-sizing: border-box;"