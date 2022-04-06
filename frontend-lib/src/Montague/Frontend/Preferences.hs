
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core
import Data.Functor

data Preferences = Preferences {
    style :: Style
}

preferencePage :: _ => m (Dynamic t Preferences)
preferencePage = do
    p $ text "Hello"
    checkboxValue <- el "form" $ el "p" $ el "label" $ do
        el "span" $ text "Filled in"
        _checkbox_value <$> checkbox True def
    pure $ checkboxValue <&> 
      (\case
        True  -> Android
        False -> IOS) <&>
      Preferences