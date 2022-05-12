
module Bedelibry.Parser.Markdown where

import Text.Parsec hiding (token)
import qualified Data.Text as T
import Data.Functor

-- | Specailized markdown parser allowing for annotations
-- in a logic programming language.

newtype MdDoc a =
    MdDoc [MdSection a]
        deriving(Show)

data MdSection a =
    BigHeader T.Text
  | SmallHeader T.Text
  | Paragraph (MdParagraph a)
      deriving(Show)

type MdParagraph a = [MdParagraphComponent a]

data MdParagraphComponent a =
    RawText T.Text
  | Italic (MdParagraphComponent a)
  | Bold (MdParagraphComponent a)
  | Annotated a T.Text
      deriving(Show)

token :: Parsec String () t -> Parsec String () t
token p = do
    whitespace
    res <- p
    whitespace
    return res

whitespace = do
    many $
        try (char ' ' >> pure ()) <|>
            (char '\n' >> pure ())
    pure ()

document :: Parsec String () (MdDoc ())
document = MdDoc <$>
    section `sepBy` (char '\n' >> whitespace)

section :: Parsec String () (MdSection ())
section = try smallHeader <|>
    try bigHeader <|>
    Paragraph <$> paragraph

smallHeader :: Parsec String () (MdSection ())
smallHeader = SmallHeader <$> do
    token (string "##")
    T.pack <$> many1 (try $ noneOf "\n")

bigHeader :: Parsec String () (MdSection ())
bigHeader = BigHeader <$> do
    token (string "#")
    T.pack <$> many1 (try $ noneOf "\n")

paragraph :: Parsec String () (MdParagraph ())
paragraph = (: []) <$>
    paragraphComponent

paragraphComponent :: Parsec String () (MdParagraphComponent ())
paragraphComponent = RawText . T.pack <$>
    many1 (try $ noneOf "\n")