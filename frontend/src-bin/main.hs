
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Montague.Frontend
import Frontend.Obelisk
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

main :: IO ()
main = mainWidgetWithHead header body
