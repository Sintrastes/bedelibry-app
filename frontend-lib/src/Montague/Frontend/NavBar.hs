{-# LANGUAGE PartialTypeSignatures, ImplicitParams, OverloadedStrings, LambdaCase, RecursiveDo, FlexibleContexts, DataKinds, GADTs #-}

module Montague.Frontend.NavBar where

import qualified Data.Text as T
import Montague.Frontend.Utils
import Reflex.Dom.Core
import Control.Monad
import Data.Functor

class HasIcon e where
    iconUrl :: e -> T.Text

iOSNavBar :: _ => Dynamic t Style -> [e] -> m (Event t e)
iOSNavBar style tabs =
  let ?style = style in
  let navAttrs = style <&> (\case
        IOS     -> "class" =: "p-mobile-tabs"
        Android -> "style" =: "display: none;")
  in
     elDynAttr "div" navAttrs $ do
         menuEvents <- forM tabs $ \tab -> do
            btnEvents <- iOSNavButton (T.pack $ show tab) (iconUrl tab)
            pure $ tab <$ btnEvents
         pure $ leftmost menuEvents

-- iOSNavButton :: DomBuilder t m => T.Text -> T.Text -> m (DomEventType t 'ClickTag)
iOSNavButton label iconUrl = el "div" $
    domEvent Click . fst <$>
        elAttr' "a" ("data-p-mobile-toggle" =: ("#" <> label)) (do
            elAttr "img" ("src" =: iconUrl <> "width" =: "19") $ pure ()
            p $ text label)

-- | Nav bar widget. Only shown with an Android style enabled.
androidNavBar :: _ => Dynamic t Style -> [e] -> m (Event t e)
androidNavBar style tabs = let ?style = style in mdo
    let navAttrs = ?style <&> \case
            Android -> "class" =: "nav-wrapper light-blue darken-1"
            IOS     -> "style" =: "display: none;"

    (navBarEvents, toggleMenuEvent) <- elDynAttr "nav" navAttrs $ el "div" $ do
        elAttr "a" ("class" =: "brand-logo" <> "style" =: "padding-left: 1em;") $ text "Montague"
        navMenu <- elAttr' "a" ("class" =: "sidenav-trigger") $
            elClass "i" "material-icons" $ text "menu"
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
            menuEvents <- forM tabs (\tab -> do
                btnEvents <- navButton (T.pack $ show tab)
                pure $ tab <$ btnEvents)

            pure (leftmost menuEvents, domEvent Click (fst navMenu))

    sidebarOpened <- accumDyn (\s _ -> not s) False
        (leftmost [() <$ toggleMenuEvent, () <$ navPaneEvents])

    let sidebarAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
            if isOpened
                then "style" =: "display: block; z-index: 999;"
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
        elClass' "a" "w3-bar-item w3-button"
            (text x)

navButton x = li $
    domEvent Click . fst <$>
        el' "a" (text x)