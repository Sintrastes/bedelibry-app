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

module Montague.Frontend.Pages.Home where

import Prelude hiding ((<=), div)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Montague.Frontend.Utils
import Control.Exception
import Reflex.Dom.Core hiding (button, tabDisplay, Home)
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Montague.Types
import Montague
import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

homePage :: _ => Dynamic t (Maybe SomeLexicon) -> Dynamic t Style -> m ()
homePage maybeParsedSchema style = let ?style = style in noScrollPage $ do
    p $ text "Enter in a sentence you want to parse!"

    inputText <- textEntry

    let parsed = do
            schema <- maybeParsedSchema
            case schema of
                Nothing -> pure Nothing
                Just schema -> do
                    text <- T.unpack <$> _inputElement_value inputText
                    pure $ getParseFromLexicon schema show text

    let isValid = isJust <$> parsed

    let parsedDisplay = T.pack .
            fromMaybe "Could not parse" <$>
            parsed

    let parsedFmt = isValid <&> (\case
            True  -> mempty
            False -> "class" =: "red-text")

    elDynAttr "p" parsedFmt $ do
        dynText parsedDisplay