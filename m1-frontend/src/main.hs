
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures #-}

import Bedelibry.Frontend
import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp

header :: _ => m ()
header = do
  el "title" $ text "Montague"
  elAttr "meta" (
    "name" =: "viewport" <>
    "content" =: "width=device-width, initial-scale=1") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/w3.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "script" (
    "src" =: "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js") blank
  elAttr "script" (
    "src" =: "https://cdn.jsdelivr.net/npm/feather-icons/dist/feather.min.js") blank
  elAttr "link" (
    "id" =: "css-style" <>
    "href" =: "https://sintrastes.github.io/demos/montague/materialize.min.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/main.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/pane.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "id" =: "material-colors" <>
    "href" =: "https://sintrastes.github.io/demos/montague/material-colors-default.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "script" (
    "src" =: "https://sintrastes.github.io/demos/montague/pane.js") blank

main = run 3911 $ mainWidgetWithHead header body