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

module Bedelibry.Frontend.Pages.Entity where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Bedelibry.Frontend.Strings as Strings
import Bedelibry.Frontend.Utils
import Data.Proxy
import Bedelibry.Frontend.Route (Route)
import qualified Bedelibry.Frontend.Route as Route
import Reflex.Dom.Core hiding (button, select, checkbox)
import Montague.Types
import Montague.Lexicon hiding (enumValues)
import Montague.Semantics
import Data.List
import Data.Functor
import Data.Monoid
import Control.Monad.Tree
import Control.Monad
import Control.Monad.Fix
import Reflex.DynamicWriter.Base

entityPage ::  _ => Event t () -> Event t () -> Dynamic t PreferenceData -> Dynamic t (Maybe SomeLexicon) -> DynamicWriterT t [Event t Route] m ()
entityPage addBtnClicks filterBtnClicks prefs maybeParsedSchema = let ?prefs = prefs in let ?style = stylePref <$> prefs in scrollPage $ elAttr "section" ("data-role" =: "list") $ do
    modal addBtnClicks $ do
        modalHeader "Add a new entity"

        select "Type" ["Person", "Place", "Thing"] "Person"
        labeledTextEntry $ T.pack $ show Strings.Name
        labeledTextArea $ T.pack $ show Strings.Description

        pure $ pure ()
    
    dyn $ maybeParsedSchema <&> \case
        Nothing -> elAttr "div" centerContent $
            el "div" $ do
                elClass "p" grayText $ text "No schema has been specified."
                elClass "p" grayText $ do
                    text "You need to enter one in "
                    textLink "the schema page" Route.Schema
                    text "."
        Just (SomeLexicon pT pA getDocs _ semantics) -> do
            let entities = getEntities pA
            let types = fmap (bfs . typeOfAtom semantics) entities
            let typingPairs = zip entities types

            filterDialog filterBtnClicks types

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
            elAttr "p" ("style" =: "float: right;") $ textLink
                (T.pack $ intercalate " | " . fmap show $ typ)
                (Route.Types Nothing)
            el "p" $ text $ T.pack $ getDocs entity
        _ -> elClass "li" "collection-item" $ do
            el "span" $ text $
                T.pack $ show entity <> ": "
            elAttr "span" ("style" =: "float: right;") $ textLink 
                (T.pack $ intercalate " | " . fmap show $ typ)
                (Route.Types Nothing)
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

filterDialog :: _ => Event t () -> [typ] -> m (Dynamic t (Endo [a]))
filterDialog onClick types = do
    submitEvent <- modal onClick $ do
        modalHeader "Filter entities"
        forM types \x ->
            checkbox (T.pack $ show x) True
        return $ pure $ Endo id

    holdDyn (Endo id) 
        (mapMaybe id submitEvent)
