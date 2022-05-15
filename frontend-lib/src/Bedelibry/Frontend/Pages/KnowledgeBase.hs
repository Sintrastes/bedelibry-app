{-# LANGUAGE ImplicitParams, RecursiveDo, OverloadedStrings, PartialTypeSignatures, LambdaCase, ScopedTypeVariables #-}

module Bedelibry.Frontend.Pages.KnowledgeBase where

import Reflex.Dom.Core hiding (button, tabDisplay)
import qualified Data.Text as T
import System.Info
import Data.Version
import Bedelibry.Frontend.Utils
import Data.Maybe
import qualified Bedelibry.Frontend.Strings as Strings
import Data.Functor
import Control.Exception
import Control.Monad.IO.Class

import Ast
import Parser

knowledgeBaseName :: String
knowledgeBaseName = "/data/kb/ruleset.pl"

knowledgeBasePage :: _ => Dynamic t PreferenceData -> FilePath -> m (Dynamic t (Maybe Program))
knowledgeBasePage prefs montagueDir = let ?style = stylePref <$> prefs in noScrollPage $ mdo
    -- Load the knowledge base from disk.
    kbSchemaText <- liftFrontend "" $
        catch (readFile (montagueDir <> knowledgeBaseName))
            (\(e :: SomeException) -> return "")

    let textAttrs = "spellcheck" =: "false" <>
          "class" =: "input-field col s12" <> 
          "style" =: "display:flex;height:75%;width:100%;"

    el "p" $ text "Enter in the facts for your data"

    kbText <- elAttr "div" textAttrs $ textAreaElement (
        def & textAreaElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ ("class" =: "p-form-text p-form-no-validate" <>
                 "style" =: ("resize: none;height: 100%;width:100%;" <> boxResizing))
            & textAreaElementConfig_initialValue
            .~ T.pack kbSchemaText
     )

    small $ elAttr "p" ("style" =: "margin-left: 1em;" <> "class" =: "unselectable") $
        dynText $ parsedClauses <&> (\case
            Left e  -> "❌ " <> T.pack (show Strings.InvalidRuleset) <> ": " <> T.pack (show e)
            Right x -> "✅ " <> T.pack (show Strings.RulesetValid) <> ".")

    saveEvent <- button "save"

    let parsedClauses = clausesFromString "" . T.unpack <$> 
            _textAreaElement_value kbText

    -- Persit the knowledge base to disk upon pressing save
    prerender (pure never) $ performEvent $ saveEvent <&> (\_ -> do
        latestKbText <- sample $ current $ T.unpack <$>
              _textAreaElement_value kbText
        res <- liftIO $ try $
            writeFile (montagueDir <> knowledgeBaseName) latestKbText

        case res of
            Left (e :: IOException)  -> toast $ "Error saving ruleset: " <> T.pack (show e)
            Right _ -> toast "Saved ruleset")

    pure $ eitherToMaybe <$> parsedClauses
  where boxResizing = "-webkit-box-sizing: border-box;"
          <> "-moz-box-sizing: border-box;"
          <> "box-sizing: border-box;"