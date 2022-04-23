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

module Montague.Frontend.Entity where

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

entityPage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
entityPage style maybeParsedSchema = let ?style = style in scrollPage $ do
    modalEvent <- elAttr "div" ("style" =: "display: flex; justify-content: flex-end;") $
        button "New Entity"

    modal modalEvent $ do
        el "h4" $ text "Add a new entity"
        el "p" $ text "Hello, world"
        pure $ pure ()
    
    dyn $ maybeParsedSchema <&> \case
        Nothing -> elAttr "div" centerContent $
            el "div" $ do
                elClass "p" grayText $ text "No schema has been specified."
                elClass "p" grayText $ text "You need to enter one in the schema page."
        Just (SomeLexicon pT pA getDocs semantics) -> do
            let entities = getEntities pA
            let types = fmap (bfs . typeOfAtom semantics) entities
            let typingPairs = zip entities types
            case entities of
                [] -> elAttr "div" centerContent $
                    el "div" $ do
                        elClass "p" grayText $ text "No entities have been specified."
                _ -> elClass "ul" "collection" $ 
                    forM_ typingPairs (\(entity, typ) -> do
                        elClass "li" "collection-item" $ do
                            el "span" $ text $
                                T.pack $ show entity <> ": "
                            elAttr "span" ("style" =: "float: right;") $ text $
                                T.pack $ intercalate " | " . fmap show $ typ
                            el "p" $ text $ T.pack $ getDocs entity)
    pure ()
  where
    getEntities :: (Bounded a, Enum a) => Proxy a -> [a]
    getEntities Proxy = enumValues

    grayText = "unselectable grey-text text-lighten-1"

    centerContent =
        "style" =: ("text-align: center;" <>
            "min-height:100%;" <> 
            "display:flex;" <> 
            "justify-content:center;" <> 
            "align-items:center;")

enumValues :: (Bounded a, Enum a) => [a]
enumValues = [minBound..maxBound]
