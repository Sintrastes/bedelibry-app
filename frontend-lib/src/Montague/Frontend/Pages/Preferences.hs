
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Montague.Frontend.Pages.Preferences where

import Montague.Frontend.Utils
import Reflex.Dom.Core hiding (checkbox, button)
import Data.Functor
import qualified Data.Text as T
import qualified Montague.Frontend.Strings as Strings
import Data.Aeson.TH
import Data.Aeson
import Data.Default
import Data.Maybe
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Fix

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

radioPref :: (PostBuild t m, MonadFix m, ?style :: Dynamic t Style, MonadHold t m, DomBuilder t m, Show a) => T.Text -> T.Text -> [a] -> a -> m (Dynamic t a)
radioPref header description values initialValue = do
    el "h5" $ text header
    el "p" $ text description
    onClick <- button "Open"
    modal onClick $ do
        el "h5" $ text header
        radioGroup header values initialValue
    pure $ pure initialValue

preferencePage :: _ => Dynamic t Style -> FilePath -> m (Dynamic t PreferenceData)
preferencePage style montagueDir = let ?style = style in scrollPage $ do
    let prefsFile = montagueDir <> "/preferences.json"

    -- Load the preference data from disk.
    loadedPrefs <- loadPrefs prefsFile

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

    let dynPrefs = PreferenceData <$> 
          styleDyn <*>
          darkMode

    -- Whenever the preferences update, save it to file.
    persistPrefs dynPrefs prefsFile

    return dynPrefs
        
loadPrefs prefsFile = do
    loadedPrefs :: PreferenceData <- liftFrontend def $
        catch (fromJust <$> decodeFileStrict prefsFile)
            (\(e :: SomeException) -> return def)
    pure loadedPrefs

persistPrefs dynPrefs prefsFile = 
    prerender (pure never) $ performEvent $ updated dynPrefs <&> 
        \newPrefs ->
            liftIO $ encodeFile prefsFile newPrefs

