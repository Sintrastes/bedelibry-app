
import Frontend
import Reflex.Dom.Core
import Language.Javascript.JSaddle

header :: _ => m ()
header = do
  el "title" $ text "Montague"
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/w3.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/materialize.min.css" <>
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

main = run 3911 $ mainWidgetWithHead header body