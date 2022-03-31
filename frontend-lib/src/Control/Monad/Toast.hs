
module Control.Monad.Toast where

import Data.Text as T
import Reflex.Dom
import Obelisk.Frontend
import Language.Javascript.JSaddle (eval, liftJSM)

class MonadToast m where
    toast :: T.Text -> m ()

instance MonadWidget m => MonadToast m where
    toast message = do
        liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
        pure ()

instance ObeleskWidget m => MonadToast m where
    toast message = pure ()

