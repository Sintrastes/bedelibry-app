{-# LANGUAGE GADTs, RankNTypes, RecursiveDo, OverloadedStrings, FlexibleContexts #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free
import Reflex.Dom.Core hiding(Tab)
import Control.Monad.Fix
import Data.Functor
import Debug.Trace

data TabF m a where
    Tab :: T.Text -> m r -> (r -> a) -> TabF m a

instance Functor m => Functor (TabF m) where
    fmap f (Tab label widget rest) = Tab label widget (f . rest)

type Tab m = Free (TabF m)

tab :: Monad m => T.Text -> m a -> Tab m a
tab label x = liftF (Tab label x id)

tabDisplay :: (MonadFix m, MonadHold t m, DomBuilder t m, PostBuild t m) =>
     T.Text
  -> [T.Text]
  -> ([T.Text] -> m (Event t T.Text))
  -> Tab m ()
  -> m ()
tabDisplay defaultTab tabs header = tabDisplay' defaultTab tabs header wrap
  where
    displayAttrs navEvents label = navEvents <&> \selected ->
        if selected == label
            then "class" =: "column main-column" 
            else "style" =: "display: none;"
    wrap navEvents label x =
        elDynAttr "div" (displayAttrs navEvents label) x

tabDisplay' :: (MonadHold t m, MonadFix m) =>
     T.Text
  -> [T.Text]
  -> ([T.Text] -> m (Event t T.Text))
  -> (forall r. Dynamic t T.Text -> T.Text -> m r -> m r)
  -> Tab m ()
  -> m ()
tabDisplay' defaultTab tabs header wrap tab = trace "tabDisplay'" $ do
    rec 
        navEvents <- header tabs
        currentTab <- holdDyn defaultTab navEvents
        wrapComponents wrap currentTab tab
        pure ()
    pure ()

wrapComponents :: Monad m =>
    (forall r. Dynamic t T.Text -> T.Text -> m r -> m r)
 -> Dynamic t T.Text
 -> Tab m ()
 -> m ()
wrapComponents wrap navEvents (Pure _) = pure ()
wrapComponents wrap navEvents (Free (Tab label x rest)) = trace "wrapComponents" $ do
    res <- wrap navEvents label x
    labels <- wrapComponents wrap navEvents (rest res)
    pure ()
