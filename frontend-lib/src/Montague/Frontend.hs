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

import System.Info
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Reflex.Dom.Core hiding (button)
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

body :: _ => m ()
body = mdo
    style <- holdDyn Android never

    -- Nav bar
    currentPage <- navBar style

    -- App body
    elClass "div" "column main-column" $ mdo
        maybeParsedSchema <- schemaPage style

        homePage maybeParsedSchema style

        preferencePage style

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
navBar :: _ => Dynamic t Style -> m (Dynamic t NavEvent)
navBar style = let ?style = style in do
    let navAttrs = ?style <&> \case
            Android -> "class" =: "nav-wrapper light-blue darken-1"
            IOS     -> "style" =: "display: none;"

    (navBarEvents, toggleMenuEvent) <- elDynAttr "nav" navAttrs $ el "div" $ do
        elAttr "a" ("class" =: "brand-logo" <> "style" =: "padding-left: 1em;") $ text "Montague"
        navMenu <- elAttr' "a" ("class" =: "sidenav-trigger") $
            elClass "i" "material-icons" $ text "menu"
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
            homeEvents       <- navButton "Home"
            schemaEvents     <- navButton "Schema"
            preferenceEvents <- navButton "Preferences"

            pure $
              (leftmost [
                NavHome <$ homeEvents,
                NavSchema <$ schemaEvents,
                NavPrefs <$ preferenceEvents],
               domEvent Click (fst navMenu))

    sidebarOpened <- accumDyn (\s _ -> not s) False
        toggleMenuEvent

    let sidebarAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
            if isOpened
                then "style" =: "display: block; z-index: 999;"
                else "style" =: "display: none;"

    -- Nav bar menu
    navPaneEvents <- elDynAttr "div" sidebarAttrs $ ul $ do
        homeEvents       <- sidebarButton "Home"
        schemaEvents     <- sidebarButton "Schema"
        preferenceEvents <- sidebarButton "Preferences"
        pure $ leftmost [
            NavHome <$ homeEvents,
            NavSchema <$ schemaEvents,
            NavPrefs <$ preferenceEvents]

    let navEvents = leftmost [navBarEvents, navPaneEvents]

    accumDyn (\_ e -> e) NavHome
        navEvents

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
    montagueDir <- if os == "linux-android"
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

eitherToMaybe (Left e)  = Nothing
eitherToMaybe (Right x) = Just x

data Style =
    Android
  | IOS

data NavEvent =
    NavHome
  | NavSchema
  | NavPrefs
 deriving(Show)

liftFrontend d x = do
    res <- current <$> prerender (pure d) (liftIO x)
    sample res

liftFrontend' d x = do
    res <- current <$> prerender (pure d) x
    sample res

p x = el "p" $ x

div x = el "div" $ x

small x = el "small" $ x

ul x = el "ul" $ x

li x = el "li" $ x

-- | A button widget that is styled appropriately
--    depending on the currently set style.
button label = do
    let attributes = ?style <&> \case
          Android -> "class" =: "waves-effect waves-light btn light-blue"
          IOS     -> "class" =: "p-btn p-btn-mob"
    domEvent Click . fst <$> elDynAttr' "a" attributes
      (text label)

toast message = do
    liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
    pure ()

toastOnErrors x = do
    res <- x
    case res of 
        Left  e -> 
            liftFrontend' () $ 
                toast $ "An exception occured when loading Montague: " <> T.pack (show e)
        Right _ -> 
            pure ()