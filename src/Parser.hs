{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Void (Void)
import Data.Word (Word8)
import Grammar (Sum (..))
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

type Parser = Parsec Void ByteString

-- Megaparsec provides tools to get the exact source position
-- but for reasons that I don't get that's not recommended
-- (see `getSourcePos` (apparently expensive) and `pstateSourcePos`).
-- Instead, we can do that afterwards with e.g. `reachOffset`
data Span = Span Int Int
  deriving (Show, Eq)

isLower, isUpper, isUnderscore, isDigit, isAlpha_, isAlphaNum_ :: Word8 -> Bool
isLower c = c >= 97 && c <= 122
isUpper c = c >= 65 && c <= 90
isDigit c = c >= 48 && c <= 57
isUnderscore c = c == 95
isAlpha_ c = isLower c || isUpper c || isUnderscore c
isAlphaNum_ c = isLower c || isUpper c || isUnderscore c || isDigit c

lexeme :: Parser a -> Parser (Span, a)
lexeme = Lex.lexeme skip . withSpan
 where
  withSpan :: Parser a -> Parser (Span, a)
  withSpan p = do
    l <- getOffset
    a <- p
    r <- getOffset
    pure (Span l r, a)

skip :: Parser ()
skip = Lex.space space1 (Lex.skipLineComment "//") empty

word :: Parser ByteString
word = do
  h <- satisfy isAlpha_ <?> "alphabetic character"
  t <- takeWhileP (Just "alphanumeric character") isAlphaNum_
  pure $ BS.cons h t

symbol :: ByteString -> Parser ()
symbol = void . Lex.symbol skip

symbool :: ByteString -> Parser Bool
symbool s = True <$ Lex.symbol skip s <|> pure False

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep = flip sepEndBy (symbol ",")

(<||>) :: Parser (f a) -> Parser (g a) -> Parser (Sum f g a)
f <||> g = This <$> f <|> That <$> g

keyword :: ByteString -> Parser Span
keyword str = try $ do
  (span, bs) <- lexeme word
  if bs == str
    then pure span
    else fail $ unwords ["expected", BS8.unpack str, ", but got ", BS8.unpack bs]
