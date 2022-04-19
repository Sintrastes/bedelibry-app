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
    , ImplicitParams
    , TemplateHaskell
    , QuasiQuotes #-}

module Montague.Frontend where

import Montague.Frontend.Utils
import Montague.Frontend.Utils.Javascript
import Montague.Frontend.TabDisplay
import Montague.Frontend.Preferences
import Montague.Frontend.NavBar

import System.Info
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Reflex.Dom.Core hiding (button, tabDisplay, Home)
import Montague
import Data.Maybe
import Data.Function
import Data.Functor
import Prelude hiding ((<=), div)
import Control.Applicative
import Control.Monad.Tree
import Montague.Types
import Montague.Lexicon hiding (enumValues)
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
import Control.Monad.Tree

body :: _ => m ()
body = mdo
    topNavEvents <- androidNavBar (style <$> prefs) (enumValues @Page)

    let navEvents = leftmost [topNavEvents, bottomNavEvents]

    prefs <- tabDisplay defaultPage enumValues navEvents $ do
        maybeParsedSchema <- tab Schema $ schemaPage $ style <$> prefs

        tab Home $ homePage maybeParsedSchema $ style <$> prefs

        prefs <- tab Preferences $ preferencePage (style <$> prefs)

        tab Entities $ entityPage (style <$> prefs) maybeParsedSchema

        tab Types $ typePage (style <$> prefs) maybeParsedSchema

        pure prefs

    currentPage <- holdDyn defaultPage navEvents

    bottomNavEvents <- iOSNavBar currentPage (style <$> prefs) (enumValues @Page)

    prerender (pure never) $ performEvent $ updated (style <$> prefs) <&> \case
        IOS -> do
            modifyLink "css-style"
                 "https://sintrastes.github.io/demos/montague/puppertino/newfull.css"
            liftJSM $ eval ("setTimeout(function(){ feather.replace(); }, 150);" :: T.Text)
            pure ()
        Android -> modifyLink "css-style"
           "https://sintrastes.github.io/demos/montague/materialize.min.css"

    pure ()

defaultPage = Schema

enumValues :: (Bounded a, Enum a) => [a]
enumValues = [minBound..maxBound]

data Page =
    Schema
  | Home
  | Preferences
  | Entities
  | Types
    deriving(Eq, Enum, Bounded, Show)

instance DomBuilder t m => HasIcon t m Page where
    icon = \case
        Schema      -> elAttr "i" ("data-feather" =: "file-text") $ pure ()
        Home        -> elAttr "i" ("data-feather" =: "home") $ pure ()
        Preferences -> elAttr "i" ("data-feather" =: "settings") $ pure ()
        Entities    -> elAttr "i" ("data-feather" =: "book") $ pure ()
        Types       -> elAttr "i" ("data-feather" =: "edit") $ pure ()

homePage :: _ => Dynamic t (Maybe SomeLexicon) -> Dynamic t Style -> m ()
homePage maybeParsedSchema style = let ?style = style in do
    p $ text "Enter in a sentence you want to parse!"

    inputText <- textEntry

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

schemaPage :: _ => Dynamic t Style -> m (Dynamic t (Maybe SomeLexicon))
schemaPage style = let ?style = style in mdo
    -- Setup the application directory.
    montagueDir <- if "android" `isInfixOf` os
        then pure "/data/data/org.bedelibry.demos.montague"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.montague")

    -- toastOnErrors $ liftFrontend (Right ()) $ catch
    --     (do createDirectoryIfMissing True montagueDir
    --         pure $ Right ())
    --     (\(e :: SomeException) -> pure $ Left e)

    -- Load the schema from disk.
    loadedSchemaText <- liftFrontend "" $
        catch (readFile (montagueDir <> "/schema.mont"))
            (\(e :: SomeException) -> return "")

    elClass "p" "unselectable" $ 
        text "Enter in the schema for your data:"

    schemaText <- elAttr "div" ("class" =: "input-field col s12" <> "style" =: "display:flex;height:75%;width:100%;") $ textAreaElement (
        def & textAreaElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "p-form-text p-form-no-validate" <>
                 "style" =: ("height: 100%;width:100%;" <> boxResizing))
            & textAreaElementConfig_initialValue
            .~ T.pack loadedSchemaText
     )

    let parsedSchema = parseSchema .
          T.unpack <$>
            _textAreaElement_value schemaText

    let maybeParsedSchema = eitherToMaybe <$>
          parsedSchema

    div $ do
        elDynAttr "span" (savedStatus <&> \case
            True  -> "style" =: "float: right;" <> "class" =: "green-led"
            False -> "style" =: "float: right;" <> "class" =: "yellow-led") $ pure ()
        
        small $ elClass "p" "unselectable" $ dynText $ parsedSchema <&> (\case
            Left e  -> "❌ Invalid schema: " <> T.pack (show e)
            Right x -> "✅ Schema valid.")

    saveEvent <- button "save"

    let saveStatusEvents = leftmost
          [
               True  <$ saveEvent
             , False <$ updated (_textAreaElement_value schemaText)
          ]

    let isNew = loadedSchemaText == ""

    savedStatus <- foldDyn const (not isNew) saveStatusEvents

    prerender (pure never) $ performEvent $ saveEvent <&> (\_ -> do
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

entityPage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
entityPage style maybeParsedSchema = let ?style = style in do
    elClass "ul" "collection" $
        dyn $ maybeParsedSchema <&> \case
            Nothing -> pure ()
            Just (SomeLexicon pT pA semantics) -> do
                let entities = getEntities pA
                let types = fmap (bfs . typeOfAtom semantics) entities
                let typingPairs = zip entities types
                forM_ typingPairs (\(entity, typ) -> do
                    elClass "li" "collection-item" $ do
                        el "span" $ text $
                            T.pack $ show entity <> ": "
                        elAttr "span" ("style" =: "float: right;") $ text $
                            T.pack $ intercalate " | " . fmap show $ typ)
    pure ()
  where
    getEntities :: (Bounded a, Enum a) => Proxy a -> [a]
    getEntities Proxy = enumValues

typePage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
typePage style maybeParsedSchema = let ?style = style in do
    elClass "ul" "collection" $
        dyn $ maybeParsedSchema <&> \case
            Nothing -> pure ()
            Just (SomeLexicon pT pA semantics) -> do
                let types = getTypes pT
                forM_ types (\typ -> do
                    elClass "li" "collection-item" $ do
                        el "span" $ text $
                            T.pack $ show typ)
    pure ()
  where
    getTypes :: (Bounded t, Enum t) => Proxy t -> [t]
    getTypes Proxy = enumValues