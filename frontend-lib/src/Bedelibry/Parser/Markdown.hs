
module Bedelibry.Parser.Markdown where

import Text.Parsec hiding (token)
import qualified Data.Text as T

-- | Specailized markdown parser allowing for annotations
-- in a logic programming language.

newtype MdDoc a = 
    MdDoc [MdSection a]

data MdSection a =
    BigHeader T.Text
  | SmallHeader T.Text
  | Paragraph (MdParagraph a)

newtype MdParagraph a = 
    MdParagraph [MdParagraphComponent a]

data MdParagraphComponent a = 
    RawText T.Text
  | Italic (MdParagraphComponent a)
  | Bold (MdParagraphComponent a)
  | Annotated a T.Text

token :: Parsec String () t -> Parsec String () t
token p = do
    many (try (char ' ' >> pure ()) <|> (char '\n' >> pure ()))
    res <- p
    many (try (char ' ' >> pure ()) <|> (char '\n' >> pure ()))
    return res

document :: Parsec String () (MdDoc ())
document = MdDoc <$> many section

section :: Parsec String () (MdSection ())
section = try smallHeader <|>
    Paragraph <$> paragraph

smallHeader :: Parsec String () (MdSection ())
smallHeader = SmallHeader <$> do
    token (char '#')
    T.pack <$> manyTill anyChar (try $ char '\n')

paragraph :: Parsec String () (MdParagraph ())
paragraph = MdParagraph <$> 
    many paragraphComponent

paragraphComponent :: Parsec String () (MdParagraphComponent ())
paragraphComponent = RawText . T.pack <$>
    manyTill anyChar (try $ char '\n')