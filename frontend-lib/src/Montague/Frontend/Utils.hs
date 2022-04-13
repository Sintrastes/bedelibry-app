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
