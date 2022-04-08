
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core
import Data.Functor

data PreferenceData = PreferenceData {
    style :: Style,
    darkMode :: Bool
}

preferencePage :: _ => Dynamic t Style -> m (Dynamic t PreferenceData)
preferencePage style = let ?style = style in do
    let labelAttrs = style <&> (\case
            IOS -> "class" =: "p-form-switch"
            Android -> mempty)

    checkboxValue <- el "form" $ el "p" $ elDynAttr "label" labelAttrs $ do
        res <- _checkbox_value <$> checkbox True def
        el "span" $ text "Use Android Style"
        return res
    
    let styleDyn = checkboxValue <&> 
          (\case
            True  -> Android
            False -> IOS)

    return $ 
        PreferenceData <$> 
          styleDyn <*>
          pure False