
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.Monad.Toast where

import Data.Text as T
import Reflex.Dom.Core
import Language.Javascript.JSaddle (eval, liftJSM)

class MonadToast m where
    toast :: T.Text -> m ()

instance MonadWidget t m => MonadToast m where
    toast message = do
        liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
        pure ()

instance (PerformEvent t m, f ~ Performable m) => MonadToast f where
    toast message = do
        liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
        pure ()

