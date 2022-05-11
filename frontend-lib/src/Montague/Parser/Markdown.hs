
module Montague.Parser.Markdown where

import Text.Parsec
import qualified Data.Text as T

-- | Specailized markdown parser allowing for annotations
-- in a logic programming language.

data MdDoc a = MarkdownDoc [MdSection a]

data MdSection a =
    BigHeader T.Text
  | SmallHeader T.Text
  | Paragraph (MdParagraph a)

data MdParagraph a = MarkdownParagraph [MdParagraphComponent a]

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

header = undefined

document = undefined