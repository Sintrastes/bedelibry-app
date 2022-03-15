
module Montague.Lexicon where

import Montague.Types
import Montague.Semantics
import Text.Parsec hiding (token, ParseError)
import Text.ParserCombinators.Parsec.Char
import GHC.Real (odd)
import Data.Proxy
import GHC.TypeLits
import Data.PartialOrd hiding ((==))
import Data.Void
import Data.Function

------------- Public API --------------

-- | Load a montague lexicon from a file.
loadLexicon :: FilePath -> IO SomeLexicon
loadLexicon = undefined

-- | Load a montague lexicon from it's contents.
parseLexicon :: String -> Maybe SomeLexicon
parseLexicon x = undefined

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
        then Just $ SEOne sym
        else Nothing
  }
-- I think the real way to decode this would be via first parsing
-- the list into a vector.
parseTypeLexicon (x:xs) = someSymbolVal x & \case
  SomeSymbol sym -> undefined {- SomeTypeLexicon {
      parseType = combineTypeParsers
          (\i -> if i == x
              then Just $ SEOne sym
              else Nothing)
          _
  } -}

-- I need something of the form:
combineTypeParsers :: (String -> Maybe (ShowableEnum '[x])) 
  -> (String -> Maybe (ShowableEnum xs)) 
  -> (String -> Maybe (ShowableEnum (x ': xs)))
combineTypeParsers = undefined

parseSomeLexicon :: SomeTypeLexicon -> [(String, String)] -> [([String], String)] -> Either ParseError SomeLexicon
parseSomeLexicon _ [(x,y)] _ = undefined
parseSomeLexicon _ _ _ = undefined

data ShowableType (s :: Symbol) = ShowableType (Proxy s)

instance KnownSymbol s => Show (ShowableType s) where
    show (ShowableType p) = symbolVal p 

type family FMapShowableType (ss :: [Symbol]) where
    FMapShowableType '[]     = Void
    FMapShowableType (x ': xs) = Sum (ShowableType x) (FMapShowableType xs)

data Sum x y = 
      Sum1 x
    | Sum2 y

instance (Show x, Show y) => Show (Sum x y) where
    show (Sum1 x) = show x
    show (Sum2 y) = show y

data ShowableEnum (ss :: [Symbol]) where
    SEOne  :: Proxy s -> ShowableEnum '[s]
    SECons :: Proxy s -> ShowableEnum ss -> ShowableEnum (s ': ss)

instance KnownSymbol s => Show (ShowableEnum '[s]) where
    show (SEOne p) = symbolVal p

instance (KnownSymbol s, Show (ShowableEnum (s2 ': ss))) => Show (ShowableEnum (s ': s2 ': ss)) where
    show (SECons p xs) = symbolVal p

instance PartialOrd (ShowableEnum '[s]) where
    (<=) x y = True

data SomeShowableEnum = forall (ss :: [Symbol]). 
    SomeShowableEnum (ShowableEnum ss)

------------- Parsers --------------

comment = do
    char '%'
    many (char ' ')
    char '\n'

token :: Parsec String () t -> Parsec String () ()
token p = do
    many (char ' ' <|> char '\n' <|> comment)
    _ <- p
    many (char ' ' <|> char '\n' <|> comment)
    return ()

orT       = token (char '|')
equals    = token (char '=')
end       = token (char '.')
subtypeOf = token (string ":<") 
typeOfT   = token (char ':') 
comma     = token (char ',')
arrow     = token (string "-->")
typeToken = token (string "Type")

entityT = do
    lower
    many alphaNum

textT = many alphaNum

typeDeclaration = do
    typeToken
    equals
    sepBy1 orT entityT

subtypeDeclaration = do
    x <- entityT
    subtypeOf
    y <- entityT
    return (x, y)

atomDeclaration = do
    x <- entityT
    typeOfT
    y <- entityT
    return (x, y)

productionDeclaration = do
    x <- textT
    arrow
    y <- entityT
    return (x, y)

{-
montagueLexicon = do
    types <- many typeDeclaration
    let Right typeLexicon = parseTypeLexicon types

    -- Ignore this for now. This will be used
    --  when we implement subtyping.
    _ <- many subtypeDeclaraton

    atoms <- many atomDeclaration
    productions <- many productionDeclaration

    let Right lexicon = parseSomeLexicon typeLexicon atoms productions

    return $ lexicon
-}