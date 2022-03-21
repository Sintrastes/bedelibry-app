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
import Control.Lens
import Control.Lens.Operators

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Montague"
      elAttr "link" (
        "href" =: $(static "w3.css") <>
        "type" =: "text/css" <>
        "rel" =: "stylesheet") blank
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
  , _frontend_body = mdo
      -- Nav bar
      NavBar navEvents toggleMenuEvent <- el "nav" $ elClass "div" "nav-wrapper light-blue darken-1" $ do
          elAttr "a" ("class" =: "brand-logo" <> "style" =: "padding-left: 1em;") $ text "Montague"
          navMenu <- elAttr' "a" ("href" =: "#" <> "data-target" =: "mobile-demo" <> "class" =: "sidenav-trigger") $
              elClass "i" "material-icons" $ text "menu"
          elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
              homeEvents       <- el "li" $ domEvent Click . fst <$>
                  elAttr' "a" ("href" =: "#") (text "Home")
              schemaEvents     <- el "li" $ domEvent Click . fst <$>
                  elAttr' "a" ("href" =: "#") (text "Schema")
              preferenceEvents <- el "li" $ domEvent Click . fst <$>
                  elAttr' "a" ("href" =: "#") (text "Preferences")

              pure $ NavBar
                  (traceEventWith show $ leftmost [
                      NavHome <$ homeEvents,
                      NavSchema <$ schemaEvents,
                      NavPrefs <$ preferenceEvents])
                  (domEvent Click (fst navMenu))

      sidebarOpened <- accumDyn (\s _ -> not s) False
          toggleMenuEvent

      currentPage <- accumDyn (\_ e -> e) NavHome
          navEvents

      let sidebarAttrs = sidebarOpened <&> \isOpened ->
            "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
                if isOpened
                    then "style" =: "display: block;"
                    else "style" =: "display: none;"

      -- Nav bar menu (for small screens.)
      elDynAttr "div" sidebarAttrs $ el "ul" $ do
          el "li" $ elClass "a" "w3-bar-item w3-button" $ text "Home"
          el "li" $ elClass "a" "w3-bar-item w3-button" $ text "Schema"
          el "li" $ elClass "a" "w3-bar-item w3-button" $ text "Preferences"

      -- App body
      elClass "div" "column main-column" $ mdo
          el "p" $ text "Enter in the schema for your data:"

          schemaText <- elClass "div" "input-field col s12" $ textAreaElement (
              def & textAreaElementConfig_elementConfig 
                  . elementConfig_initialAttributes
                  .~ ("rows" =: "10" <> "cols" =: "72" <> 
                        "style" =: "height: auto;width: auto;resize: none;")
           )

          let parsedSchema = parseSchema .
                 T.unpack <$>
                 _textAreaElement_value schemaText

          let maybeParsedSchema = eitherToMaybe <$>
                parsedSchema

          el "div" $
              el "small" $ el "p" $ dynText $ parsedSchema <&> (\case
                  Left e  -> "❌ Invalid schema: " <> T.pack (show e)
                  Right x -> "✅ Schema valid.")

          elClass "a" "waves-effect waves-light btn light-blue" $
              text "Save"

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

          let parsedDisplay = T.pack .
               fromMaybe "Could not parse" <$>
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

data NavBar t = NavBar {
    navEvents :: Event t NavEvent,
    toggleMenuEvent :: Event t ()
}