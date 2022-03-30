
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = header
  , _frontend_body = body
  }

header :: _ => m ()
header = do
  el "title" $ text "Montague"
  elAttr "link" (
    "href" =: $(static "w3.css") <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: $(static "materialize.min.css") <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: $(static "main.css") <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
