{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

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
      el "title" $ text "Montague"
      elAttr "link" (
        "href" =: $(static "materialize.min.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
      elAttr "link" (
        "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
      elAttr "link" (
        "href" =: $(static "main.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
  , _frontend_body = do
      -- Nav bar
      navEvents <- el "nav" $ elClass "div" "nav-wrapper" $ do
          elClass "a" "brand-logo" $ text "Montague"
          elAttr "a" ("href" =: "#" <> "data-target" =: "mobile-demo" <> "class" =: "sidenav-trigger") $ 
              elClass "i" "material-icons" $ text "menu"
          elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
              homeEvents       <- el "li" $ domEvent Click <$> fst <$> 
                  (elAttr' "a" ("href" =: "#") $ text "Home")
              schemaEvents     <- el "li" $ domEvent Click <$> fst <$> 
                  (elAttr' "a" ("href" =: "#") $ text "Schema")
              preferenceEvents <- el "li" $ domEvent Click <$> fst <$> 
                  (elAttr' "a" ("href" =: "#") $ text "Preferences")

              pure $ traceEventWith show $ leftmost [
                  NavHome <$ homeEvents,
                  NavSchema <$ schemaEvents,
                  NavPrefs <$ preferenceEvents]

      -- TODO: Replace with events to 
      -- toggle the sidebar being open and closed.
      let toggleSidebar = never

      sidebarOpened <- accumDyn (\_ x -> x) False
          toggleSidebar

      currentPage <- accumDyn (\_ e -> e) NavHome
          navEvents

      -- Nav bar menu (for small screens.)
      -- TODO: Keep this in a div, and toggle visiblity based on
      -- whether or not it is open.
      -- See: https://www.w3schools.com/w3css/w3css_sidebar.asp
      elAttr "ul" ("class" =: "sidenav" <> "id" =: "mobile-demo") $ do
          el "li" $ el "a" $ text "Home"
          el "li" $ el "a" $ text "Schema"
          el "li" $ el "a" $ text "Preferences"

      -- App body
      elClass "div" "column main-column" $ mdo
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

data NavEvent =
    NavHome
  | NavSchema
  | NavPrefs
 deriving(Show)