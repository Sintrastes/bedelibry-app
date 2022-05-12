
{-# LANGUAGE PartialTypeSignatures, OverloadedStrings, ScopedTypeVariables #-}

module Bedelibry.Frontend.Pages.SchemaManagement where

import Reflex.Dom.Core
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import Bedelibry.Frontend.Utils
import qualified Data.Text as T

schemaManagementPage :: _ => FilePath -> m ()
schemaManagementPage montagueDir = do
    prerender blank $ do
        dirs <- liftIO $ listDirectory $ montagueDir <> "/data/kb"
        forM_ dirs $ \dir -> do
            elAttr "p" ("style" =: "margin:15px;") $ text $ T.pack dir
            divider
    pure ()