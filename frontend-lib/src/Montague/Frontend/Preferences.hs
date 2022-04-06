
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core
import Data.Functor

data Preferences = Preferences {
    style :: Style,
    darkMode :: Bool
}

preferencePage :: _ => m (Dynamic t Preferences)
preferencePage = do
    checkboxValue <- el "form" $ el "p" $ el "label" $ do
        el "span" $ text "Filled in"
        _checkbox_value <$> checkbox True def
    
    let style = checkboxValue <&> 
          (\case
            True  -> Android
            False -> IOS)

    return $ 
        Preferences <$> 
          style <*>
          pure False