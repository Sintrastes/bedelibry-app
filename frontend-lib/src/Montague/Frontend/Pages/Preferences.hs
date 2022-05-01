
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
import Data.Maybe hiding (mapMaybe)
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Fix

prefRow :: _ => m a -> m a
prefRow x = elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0;") x

prefHeader :: _ => T.Text -> m ()
prefHeader headerText = elAttr "p" ("style" =: "font-weight: bold;") $ text headerText

divider :: _ => m ()
divider = elClass "div" "divider" $ pure ()

checkboxPref :: _ => T.Text -> T.Text -> Bool -> m (Dynamic t Bool)
checkboxPref header description initialValue = do
    res <- prefRow $ do
        elClass "div" "col s10" $ do
            prefHeader header
            el "p" $ text description
        elAttr "div" ("class" =: "col s2 valign-wrapper" <> "style" =: "height: 7.5em;") $ 
            checkbox "" initialValue

    divider

    pure res

radioPref :: (Eq a, PostBuild t m, MonadFix m, ?style :: Dynamic t Style, MonadHold t m, DomBuilder t m, Show a) => T.Text -> T.Text -> [a] -> a -> m (Dynamic t a)
radioPref header description values initialValue = do
    modalDismissEvent <- prefRow $ elClass "div" "col s10" $ do
        prefHeader header
        el "p" $ text description
        onClick <- button "Open"
        modal onClick $ do
            el "h5" $ text header
            radioGroup header values initialValue

    divider

    let submitPrefEvent = mapMaybe id modalDismissEvent

    holdDyn initialValue 
        submitPrefEvent

preferencePage :: _ => Dynamic t PreferenceData -> FilePath -> m (Dynamic t PreferenceData)
preferencePage prefs montagueDir = let ?prefs = prefs in let ?style = stylePref <$> prefs in scrollPage $ do
    let prefsFile = montagueDir <> "/preferences.json"

    -- Load the preference data from disk.
    loadedPrefs <- loadPrefs prefsFile

    styleChecked <- checkboxPref (T.pack $ show Strings.StylePrefHeader)
        (T.pack $ show Strings.StylePrefDescription)
        (loadedPrefs & stylePref & \case
            Android -> True
            IOS     -> False)

    darkMode <- checkboxPref (T.pack $ show Strings.DarkModePrefHeader)
        (T.pack $ show Strings.DarkModePrefDescription)
        (loadedPrefs & darkMode)

    textSize <- radioPref "Set text size"
        "Set the default text size for the application"
        [Small, Medium, Large]
        (loadedPrefs & textSize)
    
    let styleDyn = styleChecked <&> \case
            True  -> Android
            False -> IOS

    let dynPrefs = PreferenceData <$> 
          styleDyn <*>
          darkMode <*>
          textSize

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

