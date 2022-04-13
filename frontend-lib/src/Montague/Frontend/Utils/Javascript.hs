
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

module Montague.Frontend.Utils.Javascript where

import qualified Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)

-- | Remove the element with the specified ID from the DOM.
removeWithId :: _ => T.Text -> m ()
removeWithId id = do
    liftJSM $ eval (
        (" var element = document.getElementById(" <> id <> "); \
        \ element.parentNode.removeChild(element); \
        \") :: T.Text)
    pure ()

-- | Modify the URL of the link tag with the given id.
modifyLink :: _ => T.Text -> T.Text -> m ()
modifyLink id url = do
    liftJSM $ eval (
        (" consolve.log(\"modifyLink\"); var element = document.getElementById('" <> id <> "'); \
        \ element.setAttribute('href','" <> url <> "'); \
        \") :: T.Text)
    pure ()

