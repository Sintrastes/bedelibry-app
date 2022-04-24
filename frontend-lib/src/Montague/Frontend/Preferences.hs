
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core hiding (checkbox)
import Data.Functor
import qualified Data.Text as T

data PreferenceData = PreferenceData {
    style :: Style,
    darkMode :: Bool
}

checkboxPref :: _ => T.Text -> T.Text -> m (Dynamic t Bool)
checkboxPref header description = do
    res <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0;") $ do
        elClass "div" "col s10" $ do
            elAttr "p" ("style" =: "font-weight: bold;") $ text header
            el "p" $ text description
        elAttr "div" ("class" =: "col s2 valign-wrapper" <> "style" =: "height: 7.5em;") $ 
            checkbox ""

    elClass "div" "divider" $ pure ()

    pure res

preferencePage :: _ => Dynamic t Style -> m (Dynamic t PreferenceData)
preferencePage style = let ?style = style in scrollPage $ do
    checkboxValue <- checkboxPref "Use Android style" 
        "Specify whether or not to use the Android theme."
    
    let styleDyn = checkboxValue <&> \case
            True  -> Android
            False -> IOS

    return $ 
        PreferenceData <$> 
          styleDyn <*>
          pure False