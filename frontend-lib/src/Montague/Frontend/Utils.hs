{-# LANGUAGE LambdaCase
    , MultiParamTypeClasses
    , FunctionalDependencies
    , ScopedTypeVariables
    , TypeApplications
    , DataKinds
    , FlexibleInstances
    , FlexibleContexts
    , RecursiveDo
    , BlockArguments
    , OverloadedStrings
    , ConstraintKinds
    , GADTs
    , PartialTypeSignatures
    , ImplicitParams
    , RecursiveDo #-}

module Montague.Frontend.Utils where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex.Dom.Core hiding (button, checkbox)
import qualified Reflex.Dom.Core as RD
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Data.Default
import Control.Lens.Operators
import Data.Map
import Control.Monad.Fix
import Control.Monad

class MonadNav r t m where
    writeNavEvents :: Event t r -> m ()

eitherToMaybe (Left e)  = Nothing
eitherToMaybe (Right x) = Just x

liftFrontend d x = do
    res <- current <$> prerender (pure d) (liftIO x)
    sample res

liftFrontend' d x = do
    res <- current <$> prerender (pure d) x
    sample res

p x = el "p" $ x

div x = el "div" $ x

small x = el "small" $ x

ul x = el "ul" $ x

li x = el "li" $ x

noScrollPage x = elClass "div" "column main-column" x

scrollPage x = elAttr "div" ("class" =: "column") x

data Style =
    Android
  | IOS

-- | A button widget that is styled appropriately
--    depending on the currently set style.
button label = do
    let attributes = ?style <&> \case
          Android -> "class" =: "waves-effect waves-light btn light-blue"
          IOS     -> "class" =: "p-btn p-btn-mob"
    domEvent Click . fst <$> elDynAttr' "a" attributes
      (text label)

-- | A widget for selecting a single item from a list of items.
select :: _ => Show a => T.Text -> [a] -> a -> m (Dynamic t a)
select label items initialValue = elClass "div" "input-field col s12" $ mdo
    (form, changeSelection) <- elClass "div" "select-wrapper" $ do
        (form, _) <- el' "div" $ inputElement $ def
            -- TODO: Causes issues with jsaddle.
            -- & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ inputAttrs
            & inputElementConfig_initialValue .~ T.pack (show initialValue)
            & inputElementConfig_setValue .~ (T.pack . show <$> changeSelection)

        changeSelection <- elDynAttr "ul" selectAttrs $
            leftmost <$> forM items (\item -> do
                el "li" $
                   (item <$) . domEvent Click . fst <$> el' "span" (
                        text $ T.pack $ show item))

        elSvg "svg" ("class" =: "caret" <>
            "height" =: "24" <>
            "viewBox" =: "0 0 24 24" <>
            "width" =: "24" <>
            "xmlns" =: "http://www.w3.org/2000/svg") $ do
                elSvg "path" ("d" =: "M7 10l5 5 5-5z") $ pure ()
                elSvg "path" ("d" =: "M0 0h24v24H0z" <> "fill" =: "none") $ pure ()
        
        pure (form, changeSelection)

    elAttr "label" ("style" =: "left: 0rem;") $ text label

    let selectedStyle = "display: block;" <>
          "width: 100%;" <> "left: 0px;" <>
          "top: 0px;" <> "height: auto;" <> "transform-origin: 0px 0px;" <>
          "opacity: 1;" <> "transform: scaleX(1) scaleY(1);"

    let inputClicks = form &
            domEvent Click

    dropdownOpenDyn <- foldDyn const False $
        leftmost [
            True <$ inputClicks, 
            False <$ changeSelection
        ]

    let selectAttrs = dropdownOpenDyn <&> \dropdownOpen ->
            "class" =: "dropdown-content select-dropdown" <>
                if dropdownOpen
                    then "style" =: selectedStyle
                    else empty

    -- TODO: Seems like not needed. But setting as initialAttrs
    -- causes issues with jsaddle-down.
    -- let inputAttrs = 
    --         "class" =:
    --             "select-dropdown dropdown-trigger" <>
    --             "type" =: "text" <> "readonly" =: "true"

    dynResult <- foldDyn const initialValue 
        changeSelection

    pure dynResult

elSvg tag a1 a2 = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") tag (constDyn a1) a2
  return ()

checkbox :: _ => T.Text -> m (Dynamic t Bool)
checkbox label = do
    let labelAttrs = ?style <&> (\case
            IOS -> "class" =: "p-form-checkbox-cont"
            Android -> mempty)
    el "form" $ el "p" $ elDynAttr "label" labelAttrs $ do
        res <- _checkbox_value <$> RD.checkbox True def
        el "span" $ pure ()
        text label
        return res

textLink :: _ => T.Text -> r -> m ()
textLink txt navTo = do
    clickEvents <- domEvent Click . fst <$>
        elClass' "a" "text-link" (text txt)

    writeNavEvents (navTo <$ clickEvents)

    pure ()

-- | Helper function to open a simple Ok/Cancel modal dialog.
modal :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
      => Event t () -> m (Dynamic t a) -> m (Event t (Maybe a))
modal onClick contents = mdo
    (res, onCancel, onSubmit) <- elDynAttr "div" modalAttrs $ do
        res <- elClass "div" "modal-content"
            contents

        (onCancel, onSubmit) <- elClass "div" "modal-footer" $ do
            onCancel <- domEvent Click . fst <$>
                elClass' "a" "modal-close waves-effect waves-green btn-flat" (
                    text "Cancel")
            onSubmit <- domEvent Click . fst <$>
                elClass' "a" "modal-close waves-effect waves-green btn-flat" (
                    text "Ok")

            pure (onCancel, onSubmit)

        pure (res, onCancel, onSubmit)

    -- "Overlay" for darkening the rest of the screen when the modal is open.
    elDynAttr "div" overlayAttrs $ pure ()

    let events = leftmost
            [
                Open   <$ onClick,
                Closed <$ onCancel,
                Closed <$ onSubmit
            ]

    modalVisibility <- foldDyn const Closed events

    let modalAttrs = modalVisibility <&> \case
            Closed -> "style" =: "display: none;"
            Open   -> "class" =: "modal open" <>
                "style" =: ("overflow: visible;" <> "z-index: 1003;" <>
                    "display: block;" <> "opacity: 1;" <>
                    "top: 10%;" <> "transform: scaleX(1) scaleY(1);")

    let overlayAttrs = modalVisibility <&> \case
            Closed -> empty
            Open   -> "class" =: "modal-overlay" <>
                "style" =: "z-index: 1002; display: block; opacity: 0.5;"

    pure $ leftmost
      [
        Just <$> tag (current res) onSubmit
      , Nothing <$ onCancel
      ]

data ModalEvent =
      Open
    | Closed

-- | A styled text entry from Materialize.css with a label.
labeledTextEntry :: _ => T.Text -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextEntry label = elClass "div" "input-field col" $ do
    res <- textEntry
    elAttr "label" ("class" =: "active" <> "style" =: "left: 0rem;") $ 
        text label
    pure res

-- | A styled text area from Materialize.css with a label.
labeledTextArea :: _ => T.Text -> m (InputElement EventResult (DomBuilderSpace m) t)
labeledTextArea label = labeledTextEntry label

textEntry :: _ => m (InputElement EventResult (DomBuilderSpace m) t)
textEntry = mdo
    let attrUpdateStream = ?style <&>
            attrsFromStyle &
            updated

    res <- p $ inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ mempty
            & inputElementConfig_elementConfig
            . elementConfig_modifyAttributes
            .~ attrUpdateStream)

    pure res
  where
    attrsFromStyle = \case
        Android -> "class" =: Nothing
        IOS     -> "class" =: Just "p-form-text p-form-no-validate"

toast message = do
    liftJSM $ eval ("console.log(\"toast\"); M.toast({html: '" <> message <> "'})" :: T.Text)
    pure ()

toastOnErrors x = do
    res <- x
    case res of
        Left  e ->
            liftFrontend' () $
                toast $ "An exception occured when loading Montague: " <> T.pack (show e)
        Right _ ->
            pure ()
