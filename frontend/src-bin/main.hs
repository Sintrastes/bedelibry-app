
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

import Frontend
import Frontend.Obelisk
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

instance ObeliskWidget t route m => MonadToast (Performable m) where
    toast message = do
        prerender_ blank $ 
            liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
        pure ()

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
