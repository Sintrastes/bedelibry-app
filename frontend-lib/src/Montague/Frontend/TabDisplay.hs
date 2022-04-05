{-# LANGUAGE GADTs, RankNTypes #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free

data TabF m a where
    Tab :: T.Text -> m r -> (r -> m a) -> TabF m a

instance Functor (TabF m) where
    fmap f (Tab label widget rest) = Tab label widget (\x -> f <$> rest x)

type Tab m = Free (TabF m)

-- Get the labels associated with the tabs.
labels :: Tab m a -> [T.Text]
labels (Pure _) = []
labels (Free (Tab label _ rest)) = label:labels rest

wrapComponents :: Monad m => (forall r. T.Text -> m r -> m r) -> Tab m () -> m ()
wrapComponents wrap (Pure _) = pure ()
wrapComponents wrap (Free (Tab label x rest)) = do
    res <- wrap label x
    wrapComponents wrap (rest res)