{-# LANGUAGE GADTs, RankNTypes, RecursiveDo, OverloadedStrings, FlexibleContexts #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free
import Reflex.Dom.Core hiding(Tab)
import Control.Monad.Fix
import Data.Functor

data TabF e m a where
    Tab :: e -> m r -> (r -> a) -> TabF e m a

instance Functor m => Functor (TabF e m) where
    fmap f (Tab label widget rest) = Tab label widget (f . rest)

type Tab e m = Free (TabF e m)

tab :: Monad m => e -> m a -> Tab e m a
tab label x = liftF (Tab label x id)

tabDisplay :: (Eq e, MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m) =>
     e
  -> [e]
  -> Event t e
  -> Tab e m a
  -> m a
tabDisplay defaultTab tabs navEvents = tabDisplay' defaultTab tabs navEvents wrap
  where
    displayAttrs navEvents label = navEvents <&> \selected ->
        if selected == label
            then "class" =: "column main-column" 
            else "style" =: "display: none;"
    wrap navEvents label x =
        elDynAttr "div" (displayAttrs navEvents label) x

tabDisplay' :: (MonadHold t m, MonadFix m) =>
     e
  -> [e]
  -> Event t e
  -> (forall r. Dynamic t e -> e -> m r -> m r)
  -> Tab e m a
  -> m a
tabDisplay' defaultTab tabs navEvents wrap tab = mdo
    currentTab <- holdDyn defaultTab navEvents
    wrapComponents wrap currentTab tab

wrapComponents :: Monad m =>
    (forall r. Dynamic t e -> e -> m r -> m r)
 -> Dynamic t e
 -> Tab e m a
 -> m a
wrapComponents wrap navEvents (Pure x) = pure x
wrapComponents wrap navEvents (Free (Tab label x rest)) = do
    res <- wrap navEvents label x
    labels <- wrapComponents wrap navEvents (rest res)
    pure labels
