{-# LANGUAGE GADTs, RankNTypes #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free

data TabF m a where
    Tab :: T.Text -> m r -> (r -> m a) -> TabF m a

instance Functor m => Functor (TabF m) where
    fmap f (Tab label widget rest) = Tab label widget (fmap f . rest)

type Tab m = Free (TabF m)

-- Get the labels associated with the tabs.
labels :: Functor m => Tab m a -> [T.Text]
labels (Pure _) = []
labels (Free (Tab label res rest)) = label:labels (fmap res . rest)

wrapComponents :: Monad m => (forall r. T.Text -> m r -> m r) -> Tab m () -> m ()
wrapComponents wrap (Pure _) = pure ()
wrapComponents wrap (Free (Tab label x rest)) = do
    res <- wrap label x
    rest' <- rest res
    wrapComponents wrap rest'