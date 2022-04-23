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
    , TemplateHaskell
    , QuasiQuotes #-}

module Montague.Frontend.Types where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Montague.Frontend.Utils
import Data.Proxy
import Reflex.Dom.Core hiding (button)
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Control.Monad.Tree
import Control.Monad
import Montague.Frontend.Entity

typePage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
typePage style maybeParsedSchema = let ?style = style in scrollPage $ do
    modalEvent <- elAttr "div" ("style" =: "display: flex; justify-content: flex-end;") $
        button "New Type"

    modal modalEvent $ do
        el "h4" $ text "Add a new type"
        el "p" $ text "Hello, world"
        pure $ pure ()

    dyn $ maybeParsedSchema <&> \case
        Nothing -> elAttr "div" centerContent $
            el "div" $ do 
                elClass "p" grayText $ text "No types have been specified."
                elClass "p" grayText $ text "You can add them in the schema page."
        Just (SomeLexicon pT pA _ semantics) -> elClass "ul" "collection" $ do
            let types = getTypes pT
            forM_ types (\typ -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show typ)
    pure ()
  where
    getTypes :: (Bounded t, Enum t) => Proxy t -> [t]
    getTypes Proxy = enumValues

    grayText = "unselectable grey-text text-lighten-1"

    centerContent =
        "style" =: ("text-align: center;" <>
            "min-height:100%;" <> 
            "display:flex;" <> 
            "justify-content:center;" <> 
            "align-items:center;")