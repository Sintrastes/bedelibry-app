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
    (res, onCancel, onSubmit) <- elDynAttr "div" divAttrs $ do
        res <- contents

        (onCancel, onSubmit) <- elClass "div" "modal-footer" $ do
            onCancel <- domEvent Click . fst <$>
                elClass' "a" "modal-close waves-effect waves-green btn-flat" (
                    text "Cancel")
            onSubmit <- domEvent Click . fst <$>
                elClass' "a" "modal-close waves-effect waves-green btn-flat" (
                    text "Ok")

            pure (onCancel, onSubmit)

        pure (res, onCancel, onSubmit)

    let events = leftmost
            [
                Open   <$ onClick,
                Closed <$ onCancel,
                Closed <$ onSubmit
            ]

    divVisibility <- foldDyn const Closed events

    let divAttrs = divVisibility <&> \case
            Closed -> "style" =: "display: none;"
            Open   -> "class" =: "modal open" <>
                "style" =: "z-index: 1003; display: block; opacity: 1; top: 10%; transform: scaleX(1) scaleY(1);"
    
    pure $ leftmost 
      [
        Just <$> tag (current res) onSubmit
      , Nothing <$ onCancel
      ] 

data ModalEvent =
      Open
    | Closed


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
