{-# LANGUAGE GADTs #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free

data TabF m a where
    Tab :: T.Text -> m r -> (r -> a) -> TabF m a

instance Functor (TabF m) where
    fmap f (Tab label widget rest) = Tab label widget (f . rest)

type Tab = Free TabF