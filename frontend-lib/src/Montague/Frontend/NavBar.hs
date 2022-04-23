{-# LANGUAGE PartialTypeSignatures, ImplicitParams, MultiParamTypeClasses, OverloadedStrings, LambdaCase, RecursiveDo, FlexibleContexts, DataKinds, GADTs #-}

module Montague.Frontend.NavBar where

import qualified Data.Text as T
import Montague.Frontend.Utils
import Reflex.Dom.Core
import Control.Monad
import Data.Functor

class DomBuilder t m => HasIcon t m e where
    icon :: e -> m ()

iOSNavBar :: _ => Dynamic t e -> Dynamic t Style -> [e] -> m (Event t e)
iOSNavBar currentlySelected style tabs =
  let ?style = style in
  let widget = style <&> (\case
        IOS -> elClass "div" "p-mobile-tabs" $ do
            menuEvents <- forM tabs $ \tab -> do
               let isSelected = currentlySelected <&> (== tab)
               btnEvents <- iOSNavButton isSelected (T.pack $ show tab) (icon tab)
               pure $ tab <$ btnEvents
            pure $ leftmost menuEvents
        Android -> pure never)
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
androidNavBar :: _ => Dynamic t Style -> [e] -> m (Event t e)
androidNavBar style tabs = let ?style = style in mdo
    let navAttrs = ?style <&> \case
            Android -> "class" =: "unselectable nav-wrapper light-blue darken-1"
            IOS     -> "style" =: "display: none;"

    (navBarEvents, toggleMenuEvent) <- elDynAttr "nav" navAttrs $ el "div" $ do
        elAttr "a" ("class" =: "unselectable brand-logo" <> "style" =: "padding-left: 1em;") $ text "Montague"
        navMenu <- elAttr' "a" ("class" =: "unselectable-btn sidenav-trigger" <> "unselectable" =:"on") $
            elClass "i" "material-icons" $ text "menu"
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
            menuEvents <- forM tabs (\tab -> do
                btnEvents <- navButton (T.pack $ show tab)
                pure $ tab <$ btnEvents)

            pure (leftmost menuEvents, domEvent Click (fst navMenu))

    elDynAttr "div" overlayAttrs $ pure ()

    sidebarOpened <- accumDyn (\s _ -> not s) False
        (leftmost [() <$ toggleMenuEvent, () <$ navPaneEvents])

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

    -- Nav bar menu
    navPaneEvents <- elDynAttr "div" sidebarAttrs $ ul $ do
        tabEvents        <- forM tabs (\tab -> do
            events <- sidebarButton (T.pack $ show tab)
            pure $ tab <$ events)

        pure $ leftmost tabEvents

    pure $ leftmost [navBarEvents, navPaneEvents]

sidebarButton x = li $
    domEvent Click . fst <$>
        elClass' "a" "unselectable w3-bar-item w3-button"
            (text x)

navButton x = li $
    domEvent Click . fst <$>
        el' "a" (text x)