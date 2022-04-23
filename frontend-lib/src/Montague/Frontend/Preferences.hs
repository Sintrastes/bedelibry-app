
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core hiding (checkbox)
import Data.Functor

data PreferenceData = PreferenceData {
    style :: Style,
    darkMode :: Bool
}

preferencePage :: _ => Dynamic t Style -> m (Dynamic t PreferenceData)
preferencePage style = let ?style = style in noScrollPage $ do
    checkboxValue <- checkbox "Use Android style"
    
    let styleDyn = checkboxValue <&> \case
            True  -> Android
            False -> IOS

    return $ 
        PreferenceData <$> 
          styleDyn <*>
          pure False