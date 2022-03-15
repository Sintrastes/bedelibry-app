
module Montague.Lexicon where

import Montague.Types
import Montague.Semantics
-- import Text.Parsec
-- import Distribution.Compat.CharParsing (sepBy)
import GHC.Real (odd)
import Data.Proxy
import GHC.TypeLits
import Data.PartialOrd hiding ((==))
import Data.Void
import Data.Function

-- | Raw representation of a parsed montague lexicon file.
data LexiconFile a t = LexiconFile {
    entries :: [LexiconEntry a t]
}

data SomeLexiconFile = forall a t. SomeLexiconFile (LexiconFile a t)

-- Representation of an entry in a montague lexicon.
data LexiconEntry a t = LexiconEntry {
    words :: [String],
    types :: [Term a t]
}

-- | Load a montague lexicon from a file.
getLexicon :: FilePath -> IO (Lexicon a t)
getLexicon = undefined

parseLexicon :: (String -> a) -> String -> Maybe (LexiconFile a t)
parseLexicon parseAtom x = undefined

parseLexiconEntry :: (String -> a) -> String -> Maybe (LexiconEntry a t)
parseLexiconEntry parseAtom x = undefined

------------- Type Parsing ------------

data ParseError =
  -- Type not defined (did you mean...)
    TypeNotDefined (Maybe String)
  -- Type name must be alphanumeric starting with an uppercase letter.
  | InvalidType
  -- The given word was not found in the current lexicon
  | WordNotFound String


data SomeTypeLexicon = forall t. (Show t, PartialOrd t) => SomeTypeLexicon {
  parseType :: String -> Maybe t
}

data SomeLexicon = forall a t. (Show a, Show t, MontagueSemantics a t (AnnotatedTerm a t)) => SomeLexicon {
  entityProxy :: Proxy a
}

parseTypeLexicon :: [String] -> SomeTypeLexicon
parseTypeLexicon [x] = someSymbolVal x & \case
  SomeSymbol sym -> SomeTypeLexicon {
    parseType = \i -> if i == x
        then Just (ShowableType sym) 
        else Nothing
  }
parseTypeLexicon _ = undefined

parseSomeLexicon :: [(String, String)] -> SomeTypeLexicon -> Either ParseError SomeLexicon
parseSomeLexicon [(x,y)] = undefined
parseSomeLexicon _ = undefined

data ShowableType (s :: Symbol) = ShowableType (Proxy s)

instance PartialOrd (ShowableType s) where
    (<=) x y = True

instance KnownSymbol s => Show (ShowableType s) where
    show (ShowableType p) = symbolVal p 

type family FMapShowableType (ss :: [Symbol]) where
    FMapShowableType '[]     = Void
    FMapShowableType (x ': xs) = Sum (ShowableType x) (FMapShowableType xs)

data Sum x y = 
      Sum1 x
    | Sum2 y

data ShowableEnum (ss :: [Symbol]) = 
   ShowableEnum (FMapShowableType ss)

data SomeShowableEnum = forall (ss :: [Symbol]). 
    SomeShowableEnum (ShowableEnum ss)

------------- Parsers --------------

{-
comment = do
    char '%'
    many (char ' ')
    char '\n'

token p = do
    many (char ' ' <|> char '\n' <|> comment)
    x <- p
    many (char ' ' <|> char '\n' <|> comment)
    return x

or       = token (char '|')
equals   = token (char '=')
end      = token (char '.')
sutypeOf = token (string ":<") 
typeOf = token (string ":") 
comma    = token (char ',')
arrow    = token (string "-->")
typeToken = token (string "Type")

entity = do
    lowercase
    many alphaNum

text = many alphaNum

typeDeclaration = do
    typeToken
    equals
    sepBy1 $ do
        x <- entity
        or
        return x

subtypeDeclaration = do
    x <- entity
    subtypeOf
    y <- entity
    return (x, y)

atomDeclaration = do
    x <- entity
    typeOf
    y <- entity
    return (x, y)

productionDeclaration = do
    x <- text 
    arrow
    y <- entity
    return (x, y)

-- TODO: Need to do some validation between these steps here.
montagueSchema :: Parser SomeMontagueSchema
montagueSchema = do
    types <- many typeDeclaration
    subtypeDecs <- many subtypeDeclaraton
    atoms <- many atomDeclaration
    productions <- many productionDeclaration

-}