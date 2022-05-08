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

module Montague.Frontend.Pages.Types where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Montague.Frontend.Strings as Strings
import Montague.Frontend.Utils
import Data.Proxy
import Montague.Frontend.Route (Route)
import qualified Montague.Frontend.Route as Route
import Reflex.Dom.Core hiding (button)
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Control.Monad.Tree
import Control.Monad
import Control.Monad.Fix
import Montague.Frontend.Pages.Entity

typePage :: _ => Dynamic t PreferenceData -> Dynamic t (Maybe SomeLexicon) -> DynamicWriterT t [Event t Route] m ()
typePage prefs maybeParsedSchema = let ?prefs = prefs in let ?style = stylePref <$> prefs in scrollPage $ elAttr "section" ("data-role" =: "list") $ do
    modalEvent <- elAttr "div" ("style" =: "display: flex; justify-content: flex-end;") $
        button $ T.pack $ show Strings.NewType

    modal modalEvent $ do
        modalHeader "Add a new type"
        
        labeledTextEntry $ T.pack $ show Strings.Name
        labeledTextArea $ T.pack $ show Strings.Description

        pure $ pure ()

    dyn $ maybeParsedSchema <&> \case
        Nothing -> elAttr "div" centerContent $
            el "div" $ do 
                elClass "p" grayText $ text "No types have been specified."
                elClass "p" grayText $ do
                    text "You can add them in "
                    textLink "the schema page" Route.Schema
                    text "."
        Just (SomeLexicon pT pA _ _ semantics) -> elClass "ul" "collection" $ do
            let types = getTypes pT
            forM_ types (\typ -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show typ)
    pure ()
  where
    getTypes :: (Bounded t, Enum t) => Proxy t -> [t]
    getTypes Proxy = enumValues

    grayText = "unselectable grey-text text-lighten-1"

    centerContent =
        "style" =: ("text-align: center;" <>
            "min-height:100%;" <> 
            "display:flex;" <> 
            "justify-content:center;" <> 
            "align-items:center;")