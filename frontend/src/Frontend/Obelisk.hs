{-# LANGUAGE LambdaCase
    , MultiParamTypeClasses
    , FunctionalDependencies
    , ScopedTypeVariables
    , TypeApplications
    , DataKinds
    , FlexibleInstances
    , FlexibleContexts
    , RecursiveDo
    , TemplateHaskell
    , BlockArguments
    , OverloadedStrings
    , GADTs
    , PartialTypeSignatures
    , ImplicitParams
    , CPP #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Obelisk where

import Montague.Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex.Dom
import Data.Text as T
import Language.Javascript.JSaddle (eval, liftJSM)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = header
  , _frontend_body = body
  }

#ifdef ghcjs_HOST_OS
w3CSS = "w3.css"
#else
w3CSS = $(static "w3.css")
#endif

#ifdef ghcjs_HOST_OS
materializeCSS = $(static "materialize.min.css")
#else
materializeCSS = "materialize.min.css"
#endif

#ifdef ghcjs_HOST_OS
mainCSS = $(static "main.css")
#else
mainCSS = "main.css"
#endif

header :: _ => m ()
header = do
  el "title" $ text "Montague"
  elAttr "link" (
    "href" =: w3CSS <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "script" (
    "src" =: "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js") blank
  elAttr "link" (
    "href" =: materializeCSS <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: mainCSS <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
