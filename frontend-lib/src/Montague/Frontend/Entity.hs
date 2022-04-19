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
import Reflex.Dom.Core
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Control.Monad.Tree
import Control.Monad

entityPage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
entityPage style maybeParsedSchema = let ?style = style in do
    elClass "ul" "collection" $
        dyn $ maybeParsedSchema <&> \case
            Nothing -> pure ()
            Just (SomeLexicon pT pA semantics) -> do
                let entities = getEntities pA
                let types = fmap (bfs . typeOfAtom semantics) entities
                let typingPairs = zip entities types
                forM_ typingPairs (\(entity, typ) -> do
                    elClass "li" "collection-item" $ do
                        el "span" $ text $
                            T.pack $ show entity <> ": "
                        elAttr "span" ("style" =: "float: right;") $ text $
                            T.pack $ intercalate " | " . fmap show $ typ)
    pure ()
  where
    getEntities :: (Bounded a, Enum a) => Proxy a -> [a]
    getEntities Proxy = enumValues

enumValues :: (Bounded a, Enum a) => [a]
enumValues = [minBound..maxBound]
