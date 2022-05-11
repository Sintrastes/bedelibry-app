
{-# LANGUAGE PartialTypeSignatures, OverloadedStrings #-}

module Bedelibry.Frontend.Pages.SchemaManagement where

import Reflex.Dom.Core
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

schemaManagementPage :: _ => FilePath -> m ()
schemaManagementPage montagueDir = do
    dirs <- liftIO $ listDirectory $ montagueDir <> "/data/kb"
    forM_ dirs $ \dir ->
        el "p" $ text $ T.pack dir