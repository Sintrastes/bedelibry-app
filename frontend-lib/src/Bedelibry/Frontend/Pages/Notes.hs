
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bedelibry.Frontend.Pages.Notes where

import Reflex.Dom.Core hiding (button, tabDisplay)
import qualified Data.Text as T
import System.Info
import Data.Version
import Bedelibry.Frontend.Utils
import Data.Maybe
import qualified Bedelibry.Frontend.Strings as Strings
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Functor
import Text.Parsec (parse)
import Bedelibry.Parser.Markdown

notesPage :: (DomBuilder t m, PostBuild t m, Prerender t m) => m ()
notesPage = elAttr "div" ("class" =: "container" <> "style" =: "height: 91.5vh;") $ do
    enteredText <- elClass "div" "container__left" $ do
        _textAreaElement_value <$> textAreaElement (
            def & textAreaElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ attrs)

    let markdown = parse document "" . T.unpack <$> enteredText
    
    let renderedMarkdown = markdown <&> \case
            Left err  -> text $ T.pack $ show err
            Right doc -> renderMarkdown doc

    elAttr "div" ("class" =: "resizer" <> "id" =: "dragMe") blank

    elClass "div" "container__right" $
        dyn renderedMarkdown
    
    prerender (pure ()) $ do
        liftJSM $ eval ("setTimeout(function(){ addPaneDragListener(); }, 50)" :: T.Text)
        pure ()

    pure ()
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "text" <> "style" =: "resize: none; height: 100%; width: 100%;"

renderMarkdown :: _ => MdDoc () -> m ()
renderMarkdown (MdDoc sections) = mapM_ renderSection sections

renderSection :: _ => MdSection () -> m ()
renderSection (BigHeader t) = el "h3" $ text t
renderSection (SmallHeader t) = el "h4" $ text t
renderSection (Paragraph pg) = el "p" $ mapM_ renderParagraphComponent pg

renderParagraphComponent :: DomBuilder t m => MdParagraphComponent () -> m _
renderParagraphComponent = \case
    RawText x -> text x
    Italic x -> el "i" $ renderParagraphComponent x
    Bold x -> el "b" $ renderParagraphComponent x
    Annotated _ x -> text x



