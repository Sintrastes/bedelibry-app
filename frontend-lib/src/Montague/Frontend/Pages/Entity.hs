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

module Montague.Frontend.Pages.Entity where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Montague.Frontend.Strings as Strings
import Montague.Frontend.Utils
import Data.Proxy
import Reflex.Dom.Core hiding (button, select)
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Control.Monad.Tree
import Control.Monad

entityPage ::  _ => Dynamic t PreferenceData -> Dynamic t (Maybe SomeLexicon) -> m ()
entityPage prefs maybeParsedSchema = let ?prefs = prefs in let ?style = stylePref <$> prefs in scrollPage $ elAttr "section" ("data-role" =: "list") $ do
    modalEvent <- elAttr "div" ("style" =: "display: flex; justify-content: flex-end;") $
        button $ T.pack $ show Strings.NewEntity

    modal modalEvent $ do
        modalHeader "Add a new entity"

        select "Type" ["Person", "Place", "Thing"] "Person"
        labeledTextEntry $ T.pack $ show Strings.Name
        labeledTextArea $ T.pack $ show Strings.Description

        pure $ pure ()
    
    dyn $ maybeParsedSchema <&> \case
        Nothing -> elAttr "div" centerContent $
            el "div" $ do
                elClass "p" grayText $ text "No schema has been specified."
                elClass "p" grayText $ text "You need to enter one in the schema page."
        Just (SomeLexicon pT pA getDocs _ semantics) -> do
            let entities = getEntities pA
            let types = fmap (bfs . typeOfAtom semantics) entities
            let typingPairs = zip entities types
            case entities of
                [] -> elAttr "div" centerContent $
                    el "div" $ do
                        elClass "p" grayText $ text "No entities have been specified."
                _ -> elAttr "ul" ("class" =: "collection" <> "data-role" =: "listview") $ 
                    forM_ typingPairs (\(entity, typ) -> do
                        entityDisplay getDocs entity typ)
    pure ()
  where
    getEntities :: (Bounded a, Enum a) => Proxy a -> [a]
    getEntities Proxy = enumValues

    entityDisplay getDocs entity typ = dyn $ ?style <&> \case
        UbuntuTouch -> el "li" $ do
            el "p" $ text $
                T.pack $ show entity <> ": "
            elAttr "p" ("style" =: "float: right;") $ text $
                T.pack $ intercalate " | " . fmap show $ typ
            el "p" $ text $ T.pack $ getDocs entity
        _ -> elClass "li" "collection-item" $ do
            el "span" $ text $
                T.pack $ show entity <> ": "
            elAttr "span" ("style" =: "float: right;") $ text $
                T.pack $ intercalate " | " . fmap show $ typ
            el "p" $ text $ T.pack $ getDocs entity

    grayText = "unselectable grey-text text-lighten-1"

    centerContent =
        "style" =: ("text-align: center;" <>
            "min-height:100%;" <> 
            "display:flex;" <> 
            "justify-content:center;" <> 
            "align-items:center;")

enumValues :: (Bounded a, Enum a) => [a]
enumValues = [minBound..maxBound]
