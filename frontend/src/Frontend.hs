{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Montague
import Data.Maybe
import Data.Function
import Data.Functor
import Prelude hiding ((<=))
import Control.Applicative
import Control.Monad.Tree
import Montague.Types
import Montague.Lexicon
import Data.Proxy
import Montague.Semantics
import Data.PartialOrd hiding ((==))
import System.Environment

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Montague Android App"
      elAttr "link" (
        "href" =: $(static "materialize.min.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
      elAttr "link" (
        "href" =: $(static "main.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
  , _frontend_body = do
      elAttr "div" ("class" =: "column main-column") $ do
          el "h2" $ text "Welcome to Montague!"
          
          el "p" $ text "Enter in the schema for your data:"

          schemaText <- textAreaElement def

          let parsedSchema = parseSchema <$> 
                 T.unpack <$> 
                 _textAreaElement_value schemaText

          let maybeParsedSchema = eitherToMaybe <$> 
                parsedSchema   
          
          el "p" $ dynText $ parsedSchema <&> (\case
              Left e  -> "❌ Invalid schema: " <> (T.pack $ show e)
              Right x -> "✅ Schema valid.")  
          
          el "p" $ text "Enter in a sentence you want to parse!"

          inputText <- el "p" $ inputElement def

          let parsed = do
               schema <- maybeParsedSchema
               case schema of
                 Nothing -> pure Nothing
                 Just schema -> do
                    text <- T.unpack <$> _inputElement_value inputText
                    pure $ getParseFromLexicon schema show text 

          let isValid = isJust <$> parsed

          let parsedDisplay = T.pack <$> 
               maybe "Could not parse" id <$> 
               parsed

          let parsedFmt = isValid <&> (\case
               True  -> mempty
               False -> "class" =: "red-text")

          elDynAttr "p" parsedFmt $ do
             dynText parsedDisplay

      return ()
  }

eitherToMaybe (Left e)  = Nothing
eitherToMaybe (Right x) = Just x