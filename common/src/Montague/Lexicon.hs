
module Montague.Lexicon where

import Montague.Types
-- import Text.Parsec
-- import Distribution.Compat.CharParsing (sepBy)
import GHC.Real (odd)

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