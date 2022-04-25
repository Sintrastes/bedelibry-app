
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Montague.Frontend.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core hiding (checkbox)
import Data.Functor
import qualified Data.Text as T
import Data.Aeson.TH
import Data.Bool (Bool(True))

data PreferenceData = PreferenceData {
    stylePref :: Style,
    darkMode :: Bool
}

$(deriveJSON defaultOptions 'PreferenceData)

instance Default PreferenceData where
    def = PreferenceData {
        stylePref = Android,
        darkMode  = False
    }

checkboxPref :: _ => T.Text -> T.Text -> Bool -> m (Dynamic t Bool)
checkboxPref header description initialValue = do
    res <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0;") $ do
        elClass "div" "col s10" $ do
            elAttr "p" ("style" =: "font-weight: bold;") $ text header
            el "p" $ text description
        elAttr "div" ("class" =: "col s2 valign-wrapper" <> "style" =: "height: 7.5em;") $ 
            checkbox "" initialValue

    elClass "div" "divider" $ pure ()

    pure res

preferencePage :: _ => Dynamic t Style -> FilePath -> m (Dynamic t PreferenceData)
preferencePage style montagueDir = let ?style = style in scrollPage $ do
    -- Load the preference data from disk.
    loadedPrefs <- liftFrontend def $
        catch (readFile (montagueDir <> "/preferences.json"))
            (\(e :: SomeException) -> return def)

    styleChecked <- checkboxPref "Use Android style" 
        "Specify whether or not to use the Android theme."
        (loadedPrefs & stylePref & \case
            Android -> True
            IOS     -> False)

    darkMode <- checkboxPref "Enable dark mode"
        "Speicfy whether or not darn mode is enabled."
        (loadedPrefs & darkMode)
    
    let styleDyn = styleChecked <&> \case
            True  -> Android
            False -> IOS

    return $ 
        PreferenceData <$> 
          styleDyn <*>
          darkMode