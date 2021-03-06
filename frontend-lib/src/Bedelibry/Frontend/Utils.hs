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
    , TemplateHaskell
    , RecursiveDo #-}

module Bedelibry.Frontend.Utils where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Bedelibry.Frontend.Strings as Strings
import Reflex.Dom.Core hiding (button, checkbox)
import qualified Reflex.Dom.Core as RD
import Language.Javascript.JSaddle (eval, liftJSM)
import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Data.Default
import Control.Lens.Operators
import Data.Map hiding(filter, map)
import Control.Monad.Fix
import Control.Monad
import Data.Profunctor
import Data.Functor.Compose
import Data.Aeson.TH
import Reflex.DynamicWriter.Class
import Reflex.DynamicWriter.Base

type Form t m a b
    = Star (Compose m (Dynamic t)) a b

type SForm t m a = Form t m a a

form :: DomBuilder t m => (a -> m (Dynamic t b)) -> Form t m a b
form f = Star (Compose . f)

initForm :: DomBuilder t m => Form t m a b -> a -> m (Dynamic t b)
initForm (Star f) x = getCompose $ f x

(=.) :: Profunctor f => (x -> y) -> f y a -> f x a
(=.) = lmap

{-
-- Form API example:

data Example = Example {
    firstField :: Bool, secondField :: Bool }

exampleForm :: _ => SForm t m Example
exampleForm = Example <$>
    firstField  =. form (checkbox "First: ") <*>
    secondField =. form (checkbox "Second: ")

-}

{-
-- Idea for more flexible form building.
-- Probably need to tweak the types to get this to work.

bind :: SForm t m b -> Lens a b -> m ()
bind = undefined

mkForm :: WriterT [(a -> a)] m () -> SForm t m a

example = mkForm $ do
    form (checkbox "First") &
        bind firstField

    form (checkbox "Second") &
        bind secondField
-}

divider :: _ => m ()
divider = elClass "div" "divider" $ pure ()

data Style =
    Android
  | IOS
  | UbuntuTouch
  | Gtk
  | Windows
  | MacOS deriving(Eq)

$(deriveJSON defaultOptions ''Style)

instance Show Style where
    show Android = "Material Design Theme (Android)"
    show IOS = "iOS Theme"
    show UbuntuTouch = "Ubuntu Touch Theme"
    show Gtk = "GTK Theme (Linux)"
    show Windows = "Windows 10 Theme"
    show MacOS = "Mac OS Theme"

data TextSize =
      Small
    | Medium
    | Large
  deriving(Eq)

instance Show TextSize where
    show = \case
        Small  -> show Strings.Small
        Medium -> show Strings.Medium
        Large  -> show Strings.Large

$(deriveJSON defaultOptions ''TextSize)

data PreferenceData = PreferenceData {
    stylePref :: Style,
    darkMode  :: Bool,
    textSize  :: TextSize,
    dontShowWelcomePage :: Bool
}

$(deriveJSON defaultOptions 'PreferenceData)

instance Default PreferenceData where
    def = PreferenceData {
        stylePref = Android,
        darkMode  = False,
        textSize  = Medium,
        dontShowWelcomePage = False
    }

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

-- | A button widget that is styled appropriately
--    depending on the currently set style.
button label = do
    let attributes = ?style <&> \case
          Android     -> "class" =: "waves-effect waves-light btn"
          UbuntuTouch -> "data-role" =: "button"
          IOS         -> "class" =: "p-btn p-btn-mob"
          Gtk         -> "class" =: "gtk-btn"
    domEvent Click . fst <$> elDynAttr' "a" attributes
      (text label)

-- | A widget for selecting a single item from a list of items.
select :: _ => Show a => T.Text -> [a] -> a -> m (Dynamic t a)
select names items initialValue = do
    initialWidget <- getWidget <$> sample (current ?style)
    join <$> widgetHold initialWidget (getWidget <$> updated ?style)
  where
      getWidget = \case
          IOS   -> selectIOS names items initialValue
          _     -> selectAndroid names items initialValue


selectIOS :: (MonadHold t m, DomBuilder t m, MonadFix m, Show a) => T.Text -> [a] -> a -> m (Dynamic t a)
selectIOS label items initialValue = do
    changeSelection <- elClass "div" "p-form-select" $
        el "select" $
            leftmost <$> forM items (\item ->
                (item <$) . domEvent Click . fst <$>
                    el' "option" (text $ T.pack $
                        show item))

    foldDyn const initialValue
        changeSelection

selectAndroid :: _ => Show a => T.Text -> [a] -> a -> m (Dynamic t a)
selectAndroid label items initialValue = elClass "div" "input-field col s12" $ mdo
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

checkbox :: _ => T.Text -> Bool -> m (Dynamic t Bool)
checkbox label initialValue = do
    let labelAttrs = ?style <&> (\case
            IOS -> "class" =: "checkbox p-form-checkbox-cont"
            _   -> mempty)
    el "form" $ el "p" $ elDynAttr "label" labelAttrs $ do
        res <- _checkbox_value <$> RD.checkbox initialValue def
        elClass "span" "checkbox" $ pure ()
        text label
        return res

rangeSelect :: _ => Int -> Int -> Int -> m (Dynamic t Int)
rangeSelect minValue maxValue initialValue = do
    el "form" $
        elClass "p" "range-field" $
            elAttr "input" ("type" =: "range" <> 
                "min" =: T.pack (show minValue) <>
                "max" =: T.pack (show maxValue)) blank
    return $ pure initialValue

appText :: _ => T.Text -> m ()
appText x =  elDynAttr "p" attrs $ text x
  where attrs = ?prefs <&> textSize <&> \case
            Small  -> "class" =: "unselectable" <> "style" =: "font-size: 1em;"
            Medium -> "class" =: "unselectable" <> "style" =: "font-size: 1.2em"
            Large  -> "class" =: "unselectable" <> "style" =: "font-size: 1.45em"

radioGroup :: (MonadHold t m, DomBuilder t m, Eq a, Show a) => T.Text -> [a] -> a -> m (Dynamic t a)
radioGroup name values initialValue = do
    events <- forM values $ \value -> do
        event <- el' "p" $ el "label" $ do
            elAttr "input" ("type" =: "radio" <> "class" =: "with-gap" <> "name" =: name <>
                if value == initialValue then "checked" =: "" else empty) $
                pure ()
            el "span" $ text $ T.pack $ show value
        pure $ value <$ domEvent Click (fst event)
    holdDyn initialValue (leftmost events)

textLink :: _ => T.Text -> r -> DynamicWriterT t [Event t r] m ()
textLink txt navTo = do
    clickEvents <- domEvent Click . fst <$>
        elClass' "a" "text-link" (text txt)

    tellDyn $ pure [navTo <$ clickEvents]

    pure ()

modalHeader :: _ => T.Text -> m ()
modalHeader txt = do
    dyn $ ?style <&> \case
        Android -> elAttr "h5" ("style" =: "margin-top: 0em; margin-bottom:1em;") $ text txt
        _       -> el "h3" $ text txt
    pure ()

-- | Helper function to open a simple Ok/Cancel modal dialog.
modal :: (?style :: Dynamic t Style, MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
      => Event t () -> m (Dynamic t a) -> m (Event t (Maybe a))
modal onClick contents = mdo
    (res, onCancel, onSubmit) <- elDynAttr "div" iosOverlayAttrs $ elDynAttr "div" modalAttrs $ el "section" $ do
        res <- elClass "div" "modal-content"
            contents

        let okAttrs = "class" =: "modal-close waves-effect waves-green btn-flat" <>
                "data-role" =: "button"

        let cancelAttrs = "class" =: "negative modal-close waves-effect waves-green btn-flat" <>
                "data-role" =: "button"

        (onCancel, onSubmit) <- elClass "div" "modal-footer p-modal-button-container" $ do
            onCancel <- domEvent Click . fst <$>
                elAttr' "a" cancelAttrs
                    (text "Cancel")
            onSubmit <- domEvent Click . fst <$>
                elAttr' "a" okAttrs
                    (text "Ok")

            pure (onCancel, onSubmit)

        pure (res, onCancel, onSubmit)

    let events = leftmost
            [
                Open   <$ onClick,
                Closed <$ onCancel,
                Closed <$ onSubmit
            ]

    modalVisibility <- foldDyn const Closed events

    elDynAttr "div" overlayAttrs $ pure ()

    let modalAttrs = liftM2 (,) ?style modalVisibility <&> \case
            (Android, Closed) -> "style" =: "display: none;"
            (Android, Open)   -> "class" =: "modal open" <>
                "style" =: ("overflow: visible;" <> "z-index: 1003;" <>
                    "display: block;" <> "background-color: transparent;" <>
                    "top: 10%;" <> "transform: scaleX(1) scaleY(1);")
            (UbuntuTouch, Closed)   -> "data-role" =: "dialog" <>
                "style" =: "display: none;"
            (UbuntuTouch, Open)     -> "data-role" =: "dialog" <>
                "style" =: "display: block; height: 100vh;"
            (IOS, Closed) -> "class" =: "p-modal"
            (IOS, Open)   -> "class" =: "p-modal active"
            (Gtk, _)      -> "style" =: "display: none;"

    let iosOverlayAttrs = liftM2 (,) ?style modalVisibility <&> \case
            (IOS, Closed) -> "class" =: "p-modal-background"
            (IOS, Open)   -> "class" =: "p-modal-background nowactive"
            _ -> empty

    let overlayAttrs = liftM2 (,) ?style modalVisibility <&> \case
            (Android, Open) -> "class" =: "modal-overlay" <>
                "style" =: "z-index: 1002; display: block; opacity: 0.5;"
            _ -> empty

    pure $ leftmost
      [
        Just <$> tag (current res) onSubmit
      , Nothing <$ onCancel
      ]

data ModalEvent =
      Open
    | Closed

multiSelect :: _ => [a] -> [a] -> m (Dynamic t [a])
multiSelect options initialValue = do
    checkedDyn <- sequence <$> forM options (\option ->
        checkbox (T.pack $ show option) (option `elem` initialValue))
    return $ checkedDyn <&> \checked ->
        map fst $ filter snd $ zip options checked

-- | A styled text entry from Materialize.css with a label.
labeledTextEntry :: _ => T.Text -> m (Dynamic t (InputElement EventResult (DomBuilderSpace m) t))
labeledTextEntry label = elClass "div" "input-field col" $ do
    initialWidget <- getWidget <$> sample (current ?style)
    widgetHold initialWidget $ updated ?style <&> getWidget
  where
    getWidget = \case
        IOS -> elClass "label" "p-form-label" $ do
            text $ label <> ": "
            textEntry
        _   -> do
            res <- textEntry
            elAttr "label" ("class" =: "active" <> "style" =: "left: 0rem;") $
                text label
            pure res

-- | A styled text area from Materialize.css with a label.
labeledTextArea :: _ => T.Text -> m (Dynamic t (InputElement EventResult (DomBuilderSpace m) t))
labeledTextArea label = labeledTextEntry label

textEntry :: _ => m (InputElement EventResult (DomBuilderSpace m) t)
textEntry =
    inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs)
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "text"

autocompleteTextEntry :: _ => (T.Text -> [T.Text]) -> m (Dynamic t T.Text)
autocompleteTextEntry autocomplete = elClass "div" "input-field col s12" $ do
    res <- inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs)

    let optionsDyn = autocomplete <$> (res & _inputElement_value)

    let dropdownAttrs = "class" =: "autocomplete-content dropdown-content" <>
            "style" =: ("display: block; width: 99%;"<>
               "top: 43px;left:2px; height: auto;" <>
               "transform-origin: 0px 0px;" <>
               "opacity: 1; transform: scaleX(1) scaleY(1);")

    elAttr "ul" dropdownAttrs $ do
        dyn $ optionsDyn <&> \options->
            forM_ options $ \option -> do
                el "li" $ el "span" $ text option

    pure $ res & _inputElement_value
  where
    attrs = "type" =: "text" <>
        "class" =: "p-form-text p-form-no-validate"

-- | Builds a Dynamic that updates as the file it points
-- to updates.
fileContents :: FilePath -> m (Dynamic t String)
fileContents file = 
    -- Note that this probably needs to be implemented with
    -- something like newTriggerEvent
    -- (see https://hackage.haskell.org/package/reflex-0.8.2.0/docs/Reflex-TriggerEvent-Class.html)
    undefined

toast message = do
    liftJSM $ eval ("console.log(\"toast\"); M.toast({html: '" <> message <> "'})" :: T.Text)
    pure ()

toastOnErrors x = do
    res <- x
    case res of
        Left  e ->
            liftFrontend' () $
                toast $ "An exception occured when loading Bedelibry: " <> T.pack (show e)
        Right _ ->
            pure ()
