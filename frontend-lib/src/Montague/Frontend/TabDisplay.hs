{-# LANGUAGE GADTs, RankNTypes, RecursiveDo, OverloadedStrings, FlexibleContexts #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free
import Control.Monad.Trans.Writer
import Reflex.Dom.Core hiding(Tab)
import Control.Monad.Fix
import Data.Functor
import Reflex.DynamicWriter.Base

data TabF e m a where
    Tab :: e -> m r -> (r -> a) -> TabF e m a

instance Functor m => Functor (TabF e m) where
    fmap f (Tab label widget rest) = Tab label widget (f . rest)

type Tab t r e m = Free (TabF e m)

tab :: Monad m => e -> m a -> Tab t r e m a
tab label x = liftF (Tab label x id)

tabDisplay :: (Eq e, MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m) =>
     e
  -> Event t e
  -> Tab t r e (DynamicWriterT t [Event t r] m) a
  -> m (a, Dynamic t [Event t r])
tabDisplay defaultTab navEvents = tabDisplay' defaultTab navEvents wrap
  where
    displayAttrs navEvents label = navEvents <&> \selected ->
        if selected == label
            then mempty
            else "style" =: "display: none;"
    wrap navEvents label x =
        elDynAttr "div" (displayAttrs navEvents label) x

tabDisplay' :: (Reflex t, MonadHold t m, MonadFix m) =>
     e
  -> Event t e
  -> (forall res. Dynamic t e -> e -> DynamicWriterT t [Event t r] m res -> DynamicWriterT t [Event t r] m res)
  -> Tab t r e (DynamicWriterT t [Event t r] m) a
  -> m (a, Dynamic t [Event t r])
tabDisplay' defaultTab navEvents wrap tab = runDynamicWriterT $ mdo
    currentTab <- holdDyn defaultTab navEvents
    wrapComponents wrap currentTab tab

wrapComponents :: Monad m =>
    (forall res. Dynamic t e -> e -> DynamicWriterT t [Event t r] m res -> DynamicWriterT t [Event t r] m res)
 -> Dynamic t e
 -> Tab t r e (DynamicWriterT t [Event t r] m) a
 -> DynamicWriterT t [Event t r] m a
wrapComponents wrap navEvents (Pure x) = pure x
wrapComponents wrap navEvents (Free (Tab label x rest)) = do
    res <- wrap navEvents label x
    labels <- wrapComponents wrap navEvents (rest res)
    pure labels
