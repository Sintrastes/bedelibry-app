{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Montague.Frontend.Pages.Welcome where

import Reflex.Dom.Core hiding (checkbox)
import Montague.Frontend.Utils

welcomePage :: _ => Dynamic t PreferenceData -> m ()
welcomePage prefs = let ?style = stylePref <$> prefs in do
    elAttr "div" centerDivAttrs $ do
        elClass "p" "grey-text text-lighten-1" $ 
            text "Welcome to Montague!"
        elClass "p" "grey-text text-lighten-1" $ do
            text "To get started, select the "
            elClass "i" "material-icons tiny" $ text "menu"
            text " icon and navigate to the \"Types\" page to add some entities."
        elClass "p" "grey-text text-lighten-1" $ do
            text "For a more in-depth tutorial, visit "
            elAttr "a" ("href" =: "https://sintrastes.github.io/bedelibry/") $
                text "the tutorial."
    elAttr "div" bottomDivAttrs $ do
        checkbox "" False
        elClass "p" "grey-text text-lighten-1" $ 
            text "Don't show this page in the future"
  where
    centerDivAttrs = "style" =: ("display: flex;" 
       <> "margin: 2em;" 
       <> "flex-direction: column;" 
       <> "align-items: center;" 
       <> "justify-content: center;"
       <> "height: 72vh;")
    bottomDivAttrs = "style" =: ("display: flex;" 
       <> "margin: 2em;" 
       <> "flex-direction: row;" 
       <> "height: auto;")
