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

module Montague.Frontend.Pages.Schema where

import Prelude hiding ((<=), div)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Montague.Frontend.Utils
import Control.Exception
import Reflex.Dom.Core hiding (button, tabDisplay, Home)
import Montague.Lexicon hiding (enumValues)
import Control.Monad.IO.Class
import Data.Functor
import System.Info
import System.Directory
import Data.List

schemaPage :: _ => Dynamic t Style -> FilePath -> m (Dynamic t (Maybe SomeLexicon))
schemaPage style montagueDir = let ?style = style in noScrollPage $ mdo
    -- Load the schema from disk.
    loadedSchemaText <- liftFrontend "" $
        catch (readFile (montagueDir <> "/schema.mont"))
            (\(e :: SomeException) -> return "")

    elClass "p" "unselectable" $ 
        text "Enter in the schema for your data:"

    schemaText <- elAttr "div" ("spellcheck" =: "false" <> "class" =: "input-field col s12" <> "style" =: "display:flex;height:75%;width:100%;") $ textAreaElement (
        def & textAreaElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "p-form-text p-form-no-validate" <>
                 "style" =: ("resize: none;height: 100%;width:100%;" <> boxResizing))
            & textAreaElementConfig_initialValue
            .~ T.pack loadedSchemaText
     )

    let currentText = current $ T.unpack <$>
            _textAreaElement_value schemaText

    let lastSavedText = tag currentText saveEvent

    let parsedSchema = parseSchema .
          T.unpack <$>
            _textAreaElement_value schemaText
    
    let maybeParsedSchema = eitherToMaybe <$>
          parsedSchema

    div $ do
        elAttr "i" ("style" =: "float: right;height:18;" <> "data-feather" =: "corner-up-left") $ pure ()

        elDynAttr "span" (savedStatus <&> \case
            True  -> "style" =: "float: right;" <> "class" =: "green-led"
            False -> "style" =: "float: right;" <> "class" =: "yellow-led") $ pure ()
        
        small $ elAttr "p" ("style" =: "margin-left: 1em;" <> "class" =: "unselectable") $ 
            dynText $ parsedSchema <&> (\case
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

