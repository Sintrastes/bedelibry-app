
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Frontend
import Frontend.Obelisk
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

instance ObeleskWidget t route m => MonadToast m where
    toast message = do
        prerender_ blank $ 
            liftJSM $ eval ("M.toast({html: '" <> message <> "'})" :: T.Text)
        pure ()

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
