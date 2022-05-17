{-# LANGUAGE PartialTypeSignatures, ImplicitParams, MultiParamTypeClasses, OverloadedStrings, LambdaCase, RecursiveDo, FlexibleContexts, DataKinds, GADTs #-}

module Bedelibry.Frontend.NavBar where

import qualified Data.Text as T
import qualified Bedelibry.Frontend.Strings as Strings
import Bedelibry.Frontend.Utils
import Reflex.Dom.Core
import Control.Monad
import Data.Functor
import Bedelibry.Frontend.Route (Route, icon)
import qualified Bedelibry.Frontend.Route as Route

iOSNavBar :: _ => Dynamic t e -> Dynamic t PreferenceData -> [e] -> m (Event t e)
iOSNavBar currentlySelected prefs tabs = let ?prefs = prefs in 
  let ?style = stylePref <$> prefs in
  let widget = ?style <&> (\case
        Android -> pure never
        Gtk -> pure never
        _ -> elClass "div" "p-mobile-tabs" $ do
            menuEvents <- forM tabs $ \tab -> do
               let isSelected = currentlySelected <&> (== tab)
               btnEvents <- iOSNavButton isSelected (T.pack $ show tab) (icon tab)
               pure $ tab <$ btnEvents
            pure $ leftmost menuEvents)
  in
     switch . current <$> dynWidgetHold widget

-- dynWidgetHold :: MonadHold t m => Dynamic t (m a) -> m (Dynamic t a)
dynWidgetHold widget = do
    w <- sample $ current widget
    widgetHold w (updated widget)

-- iOSNavButton :: DomBuilder t m => T.Text -> T.Text -> m (DomEventType t 'ClickTag)
iOSNavButton isSelected label icon = el "div" $ do
    let attrs = isSelected <&> \case
          True  -> "data-p-mobile-toggle" =: ("#" <> label)
              <> "class" =: "unselectable active"
          False -> "data-p-mobile-toggle" =: ("#" <> label)
              <> "class" =: "unselectable"
    domEvent Click . fst <$> do
        elDynAttr' "a" attrs (do
            icon
            text label)

-- | Nav bar widget. Only shown with an Android style enabled.
androidNavBar :: _ => Dynamic t Route -> Dynamic t PreferenceData -> [e] -> m (Event t e, Event t ())
androidNavBar currentPage prefs tabs = let ?prefs = prefs in let ?style = stylePref <$> prefs in mdo
    let navAttrs = ?style <&> \case
            Android -> "class" =: "unselectable nav-wrapper"
            Gtk     -> "class" =: "unselectable gtk-navbar"
            _       -> "style" =: "display: none;"

    let navMenuAttrs = ?style <&> \case
            Android -> "class" =: "unselectable-btn sidenav-trigger" <> "unselectable" =: "on"
            _       -> "style" =: "display: none;"

    let filterIconAttrs = currentPage <&> \case
            Route.Types _    -> "class" =: "right"
            Route.Entities _ -> "class" =: "right"
            _                -> "style" =: "display: none;"

    let addIconAttrs = currentPage <&> \case
            Route.Types _    -> "class" =: "right"
            Route.Entities _ -> "class" =: "right"
            _                -> "style" =: "display: none;"

    let saveIconAttrs = currentPage <&> \case
            Route.Schema        -> "class" =: "right"
            Route.KnowledgeBase -> "class" =: "right"
            _                   -> "style" =: "display: none;"

    let exportIconAttrs = currentPage <&> \case
            Route.Schema        -> "class" =: "right"
            Route.KnowledgeBase -> "class" =: "right"
            _                   -> "style" =: "display: none;"

    -- Nav bar menu
    (navPane, navPaneEvents) <- elDynAttr' "div" sidebarAttrs $ ul $ do
        tabEvents        <- forM tabs (\tab -> do
            events <- sidebarButton (T.pack $ show tab)
            pure $ tab <$ events)

        pure $ leftmost tabEvents

    (navBarEvents, toggleMenuEvent, addBtnClicks) <- elAttr "div" ("data-tauri-drag-region" =: "" <> "style" =: "user-select: none;") $ elDynAttr "nav" navAttrs $ el "div" $ do
        navMenu <- elDynAttr' "a" navMenuAttrs $
            elClass "i" "material-icons" $ text "menu"
        
        elDynAttr "div" filterIconAttrs $ do
            elAttr "i" ("data-feather" =: "filter" <> "style" =: "display:block; margin: 15px;") blank
        addBtnClicks <- elDynAttr' "a" addIconAttrs $ do
            elAttr "i" ("data-feather" =: "plus" <> "style" =: "display:block; margin: 15px;") blank
        elDynAttr' "a" saveIconAttrs $ do
            elAttr "i" ("data-feather" =: "save" <> "style" =: "display:block; margin: 15px;") blank
        elDynAttr' "a" exportIconAttrs $ do
            elAttr "i" ("class" =: "material-icons") $ 
                text "import_export"
        
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "left hide-on-med-and-down") $ do
            menuEvents <- forM tabs (\tab -> do
                btnEvents <- navButton (T.pack $ show tab)
                pure $ tab <$ btnEvents)

            pure (leftmost menuEvents, domEvent Click (fst navMenu), domEvent Click (fst addBtnClicks))

    let outsideNavBarEvents = difference (domEvent Click overlay)
            (domEvent Click navPane)

    sidebarOpened <- accumDyn (\s _ -> not s) False
        (leftmost [() <$ toggleMenuEvent, () <$ navPaneEvents, () <$ outsideNavBarEvents])

    (overlay, _) <- elDynAttr' "div" overlayAttrs blank

    let sidebarAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
            if isOpened
                then "style" =: "display: block; z-index: 999;"
                else "style" =: "display: none;"
    
    let overlayAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "sidenav-overlay" <>
            if isOpened 
                then "style" =: "display: block; opacity: 1;"
                else "style" =: "display: none;"

    pure (leftmost [navBarEvents, navPaneEvents], addBtnClicks)

sidebarButton x = li $
    domEvent Click . fst <$>
        elClass' "a" "unselectable w3-bar-item w3-button"
            (text x)

navButton x = li $
    domEvent Click . fst <$>
        el' "a" (text x)