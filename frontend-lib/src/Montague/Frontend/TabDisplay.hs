{-# LANGUAGE GADTs, RankNTypes, RecursiveDo, OverloadedStrings, FlexibleContexts #-}
module Montague.Frontend.TabDisplay where

import qualified Data.Text as T
import Control.Monad.Free
import Reflex.Dom.Core hiding(Tab)
import Control.Monad.Fix
import Data.Functor

data TabF m a where
    Tab :: T.Text -> m r -> (r -> m a) -> TabF m a

instance Functor m => Functor (TabF m) where
    fmap f (Tab label widget rest) = Tab label widget (fmap f . rest)

type Tab m = Free (TabF m)

tabDisplay :: (MonadFix m, DomBuilder t m, PostBuild t m) =>
     ([T.Text] -> m (Dynamic t T.Text))
  -> Tab m ()
  -> m ()
tabDisplay header tabs = 
    tabDisplay' header wrap tabs
  where 
    displayAttrs navEvents label = navEvents <&> \selected ->
        if selected == label
            then "style" =: "display: none;"
            else mempty
    wrap navEvents label x = 
        elDynAttr "div" (displayAttrs navEvents label) x  

tabDisplay' :: MonadFix m =>
     ([T.Text] -> m (Dynamic t T.Text))
  -> (forall r. Dynamic t T.Text -> T.Text -> m r -> m r)
  -> Tab m ()
  -> m ()
tabDisplay' header wrap tab = mdo
    navEvents <- header labels
    labels <- wrapComponents wrap navEvents tab
    pure ()

wrapComponents :: Monad m => (forall r. Dynamic t T.Text -> T.Text -> m r -> m r) -> Dynamic t T.Text -> Tab m () -> m [T.Text]
wrapComponents wrap navEvents (Pure _) = pure []
wrapComponents wrap navEvents (Free (Tab label x rest)) = do
    res <- wrap navEvents label x
    rest' <- rest res
    labels <- wrapComponents wrap navEvents rest'
    pure $ label:labels
