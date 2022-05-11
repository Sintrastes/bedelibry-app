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

module Bedelibry.Frontend.Pages.Home where

import Prelude hiding ((<=), div)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Bedelibry.Frontend.Strings as Strings
import Bedelibry.Frontend.Utils
import Control.Exception
import Reflex.Dom.Core hiding (button, tabDisplay, Home)
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Montague.Types
import Montague
import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe

import Parser
import Ast
import Interp

homePage :: _ => Dynamic t (Maybe Program) -> Dynamic t (Maybe SomeLexicon) -> Dynamic t PreferenceData -> m ()
homePage  maybeParsedProgram maybeParsedSchema prefs = let ?prefs = prefs in 
    let ?style = stylePref <$> prefs in noScrollPage $ do
        -- Parsing
        appText $ T.pack $ show Strings.EnterSentence

        inputText <- autocompleteTextEntry (\x -> 
            if x /= "" 
                then filter (\el -> x `T.isPrefixOf` el) ["one", "two", "three"]
                else [])

        parsedEvent <- debounce 0.20 $ updated $ do
            schema <- maybeParsedSchema
            case schema of
                Nothing -> pure Nothing
                Just schema -> do
                    text <- T.unpack <$> inputText
                    pure $ getParseFromLexicon schema show text

        parsed <- holdDyn Nothing parsedEvent

        let isValid = isJust <$> parsed

        let parsedDisplay = T.pack .
                fromMaybe (show Strings.CouldNotParse) <$>
                parsed

        let parsedFmt = isValid <&> (\case
                True  -> mempty
                False -> "class" =: "red-text p-strawberry-500-color")

        elDynAttr "p" parsedFmt $ do
            dynText parsedDisplay

        -- Queries.
        appText $ T.pack $ show Strings.EnterQuery

        queryText <- autocompleteTextEntry (\x -> 
            if x /= "" 
                then filter (\el -> x `T.isPrefixOf` el) ["one", "two", "three"]
                else [])

        let goal = goalFromString . T.unpack . ("?- " <>) <$> queryText

        let result = do
            maybeGoal <- goal
            maybeProgram <- maybeParsedProgram
            pure $ case (maybeGoal, maybeProgram) of
                (Right g, Just p) -> T.pack $ show $
                    preformSearch p g
                _ -> ""
    
        el "p" $ dynText result 

        pure ()

preformSearch program goal = bfs $
    makeReportTree program goal
