{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Montague.Frontend.Pages.Welcome where

import Reflex.Dom.Core

welcomePage :: _ => m ()
welcomePage = elAttr "div" ("style" =: "display: flex;margin: 2em;flex-direction: column;align-items: center;justify-content: center;height: 70vh;") $ do
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