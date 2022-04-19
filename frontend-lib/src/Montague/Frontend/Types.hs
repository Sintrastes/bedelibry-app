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
import Reflex.Dom.Core
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Control.Monad.Tree
import Control.Monad
import Montague.Frontend.Entity

typePage ::  _ => Dynamic t Style -> Dynamic t (Maybe SomeLexicon) -> m ()
typePage style maybeParsedSchema = let ?style = style in do
    elClass "ul" "collection" $
        dyn $ maybeParsedSchema <&> \case
            Nothing -> pure ()
            Just (SomeLexicon pT pA semantics) -> do
                let types = getTypes pT
                forM_ types (\typ -> do
                    elClass "li" "collection-item" $ do
                        el "span" $ text $
                            T.pack $ show typ)
    pure ()
  where
    getTypes :: (Bounded t, Enum t) => Proxy t -> [t]
    getTypes Proxy = enumValues