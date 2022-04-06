{-# LANGUAGE LambdaCase
    , MultiParamTypeClasses
    , FunctionalDependencies
    , ScopedTypeVariables
    , TypeApplications
    , DataKinds
    , FlexibleInstances
    , FlexibleContexts
    , RecursiveDo
    , BlockArguments
    , OverloadedStrings
    , ConstraintKinds
    , GADTs
    , PartialTypeSignatures
    , ImplicitParams #-}

module Montague.Frontend where

import Montague.Frontend.Utils
import Montague.Frontend.TabDisplay
import System.Info
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Reflex.Dom.Core hiding (button, tabDisplay)
import Montague
import Data.Maybe
import Data.Function
import Data.Functor
import Prelude hiding ((<=), div)
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
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Fix
import System.Directory
import Data.List

body :: _ => m ()
body = mdo
    style <- tabDisplay defaultTab tabs (navBar style) $ do
        maybeParsedSchema <- tab "Schema" $ schemaPage style

        tab "Home" $ homePage maybeParsedSchema style

        tab "Preferences" $ preferencePage style

        pure $ constDyn Android
    pure ()

defaultTab :: T.Text
defaultTab = "Schema"

tabs :: [T.Text]
tabs = 
  [
    "Schema"
  , "Home"
  , "Preference"
  ]

homePage :: _ => Dynamic t (Maybe SomeLexicon) -> Dynamic t Style -> m ()
homePage maybeParsedSchema style = let ?style = style in do
    p $ text "Enter in a sentence you want to parse!"

    inputText <- p $ inputElement (
         def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "p-form-text p-form-no-validate")
      )

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

preferencePage :: _ => Dynamic t Style -> m ()
preferencePage style = let ?style = style in pure ()

-- | Nav bar widget. Only shown with an Android style.
navBar :: _ => Dynamic t Style -> [T.Text] -> m (Event t T.Text)
navBar style tabs = let ?style = style in mdo
    let navAttrs = ?style <&> \case
            Android -> "class" =: "nav-wrapper light-blue darken-1"
            IOS     -> "style" =: "display: none;"

    (navBarEvents, toggleMenuEvent) <- elDynAttr "nav" navAttrs $ el "div" $ do
        elAttr "a" ("class" =: "brand-logo" <> "style" =: "padding-left: 1em;") $ text "Montague"
        navMenu <- elAttr' "a" ("class" =: "sidenav-trigger") $
            elClass "i" "material-icons" $ text "menu"
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
            menuEvents <- forM tabs (\tab -> do
                btnEvents <- navButton tab
                pure $ tab <$ btnEvents)

            pure (leftmost menuEvents, domEvent Click (fst navMenu))

    sidebarOpened <- accumDyn (\s _ -> not s) False 
        (leftmost [() <$ toggleMenuEvent, () <$ navPaneEvents])

    let sidebarAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
            if isOpened
                then "style" =: "display: block; z-index: 999;"
                else "style" =: "display: none;"

    -- Nav bar menu
    navPaneEvents <- elDynAttr "div" sidebarAttrs $ ul $ do
        tabEvents        <- forM tabs (\tab -> do
            events <- sidebarButton tab
            pure $ tab <$ events)
       
        pure $ leftmost tabEvents

    pure $ leftmost [navBarEvents, navPaneEvents]

sidebarButton x = li $
    domEvent Click . fst <$>
        elClass' "a" "w3-bar-item w3-button"
            (text x)

navButton x = li $
    domEvent Click . fst <$>
        el' "a" (text x)

schemaPage :: _ => Dynamic t Style -> m (Dynamic t (Maybe SomeLexicon))
schemaPage style = let ?style = style in do
    -- Setup the application directory.
    montagueDir <- if "android" `isInfixOf` os 
        then pure "/data/data/org.bedelibry.demos.montague"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.montague")

    toastOnErrors $ liftFrontend (Right ()) $ catch
        (do createDirectoryIfMissing True montagueDir
            pure $ Right ())
        (\(e :: SomeException) -> pure $ Left e)

    -- Load the schema from disk.
    loadedSchemaText <- liftFrontend "" $
        catch (readFile (montagueDir <> "/schema.mont"))
            (\(e :: SomeException) -> return "")

    p $ text "Enter in the schema for your data:"

    schemaText <- elClass "div" "input-field col s12" $ textAreaElement (
        def & textAreaElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("rows" =: "10" <> "class" =: "p-form-text p-form-no-validate" <>
                 "style" =: ("height: auto;resize: none;" <> boxResizing))
            & textAreaElementConfig_initialValue
            .~ T.pack loadedSchemaText
     )

    let parsedSchema = parseSchema .
          T.unpack <$>
            _textAreaElement_value schemaText

    let maybeParsedSchema = eitherToMaybe <$>
          parsedSchema

    div $
        small $ p $ dynText $ parsedSchema <&> (\case
            Left e  -> "❌ Invalid schema: " <> T.pack (show e)
            Right x -> "✅ Schema valid.")

    saveEvent <- button "save"

    prerender (pure never) $ performEvent $ saveEvent <&> (\_ -> do
        toast $ T.pack $ "Current OS is: " <> os

        latestSchemaText <- sample $ current $ T.unpack <$>
              _textAreaElement_value schemaText
        res <- liftIO $ try $
            writeFile (montagueDir <> "/schema.mont") latestSchemaText

        case res of
            Left (e :: IOException)  -> toast $ "Error saving schema: " <> T.pack (show e)
            Right _ -> toast "Saved schema")

    pure maybeParsedSchema
  where boxResizing = "-webkit-box-sizing: border-box;"
          <> "-moz-box-sizing: border-box;"
          <> "box-sizing: border-box;"
