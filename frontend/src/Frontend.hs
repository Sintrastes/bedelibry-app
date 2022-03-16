{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route
import Montague
import Data.Maybe
import Data.Function
import Data.Functor
import Prelude hiding ((<=))
import Control.Applicative
import Control.Monad.Tree
import Montague.Types
import Montague.Lexicon
import Data.Proxy
import Montague.Semantics
import Data.PartialOrd hiding ((==))
import System.Environment

-- An example basic type to use with a schema.
data BasicType =
   Sentence
 | Question BasicType
 | Noun
 | Adjective
 | Person
 | Thing
 | ProperNoun
 | Determiner
 | DeterminedNoun
 | UndeterminedNoun deriving(Eq, Show)

instance PartialOrd BasicType where
    Person <= Noun = True
    Thing  <= Noun = True
    x <= y | x == y = True
    _ <= _ = False

data BasicAtom =
     Nate
   | Rick
   | Rachel
   | Will
   | Michael
   | Book
   | Table
   | Chair
   | Happy
   | Green
   | Sad
   | Mad
   | Mother
   | Father
   | Dog
   | Cat
   | With
   | Spoon
   | Is deriving(Show, Eq)

typeOfTerm :: Term BasicAtom BasicType -> MontagueType BasicType
typeOfTerm = \case
    Atom Nate    -> pure $ BasicType Person
    Atom Rick    -> pure $ BasicType Person
    Atom Rachel  -> pure $ BasicType Person
    Atom Will    -> pure $ BasicType Person
    Atom Michael -> pure $ BasicType Person
    Atom Book    -> pure $ BasicType Thing
    Atom Table   -> pure $ BasicType Thing
    Atom Chair   -> pure $ BasicType Thing
    Atom Is      -> pure $ ((BasicType Noun) `RightArrow` (BasicType Sentence)) `LeftArrow` (BasicType Adjective)
    Atom Happy   -> pure $ BasicType Adjective
    App (Atom Happy) [x] 
         -> pure $ BasicType Sentence
--    Atom Sad     ->
--    Atom Mad     ->
    Atom Mother  -> pure $ BasicType Person
    Atom Father  -> pure $ BasicType Person
    Atom Dog     -> pure $ BasicType Person
    Atom Cat     -> pure $ BasicType Person
-- Atom With
    Atom Spoon   -> pure $ BasicType Thing

-- Example lexicon.
myLexicon :: Lexicon BasicAtom BasicType
myLexicon = \case
    "nate"    -> pure $ Atom Nate
    "nathan"  -> pure $ Atom Nate
    "rick"    -> pure $ Atom Rick
    "rachel"  -> pure $ Atom Rachel
    "will"    -> pure $ Atom Will
    "william" -> pure $ Atom Will
    "michael" -> pure $ Atom Michael
    "book"    -> pure $ Atom Book
    "table"   -> pure $ Atom Table
    "chair"   -> pure $ Atom Chair
    "mother"  -> pure $ Atom Mother
    "mom"     -> pure $ Atom Mother
    "father"  -> pure $ Atom Father
    "dad"     -> pure $ Atom Father
    "cat"     -> pure $ Atom Cat
    "dog"     -> pure $ Atom Dog
    "spoon"   -> pure $ Atom Spoon
    "is"      -> pure $ Atom Is
    "happy"   -> pure $ Atom Happy
    _         -> empty

exampleSemantics :: MontagueSemantics BasicAtom BasicType (AnnotatedTerm BasicAtom BasicType)
exampleSemantics = MontagueSemantics {
   typeOf    = typeOfTerm,
   parseTerm = myLexicon,
   interp    = id
}

exampleLexicon = SomeLexicon (Proxy @BasicAtom) exampleSemantics

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Montague Android App"
      elAttr "link" (
        "href" =: $(static "materialize.min.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
      elAttr "link" (
        "href" =: $(static "main.css") <> 
        "type" =: "text/css" <> 
        "rel" =: "stylesheet") blank
  , _frontend_body = do
      elAttr "div" ("class" =: "column main-column") $ do
          el "h2" $ text "Welcome to Montague!"
          
          {-
          el "p" $ text "Enter in the schema for your data:"

          schemaText <- textAreaElement def

          let parsedSchema = parseSchema <$> schemaText
          -}
          
          el "p" $ text "Enter in a sentence you want to parse!"

          inputText <- el "p" $ inputElement def

          let parsed = getParseFromLexicon exampleLexicon show <$> 
               T.unpack <$> 
               _inputElement_value inputText

          let isValid = isJust <$> parsed

          let parsedDisplay = T.pack <$> 
               maybe "Could not parse" id <$> 
               parsed

          let parsedFmt = isValid <&> (\case
               True  -> mempty
               False -> "class" =: "red-text")

          elDynAttr "p" parsedFmt $ do
             dynText parsedDisplay

      return ()
  }
