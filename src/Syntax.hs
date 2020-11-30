{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import AST
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Lexer qualified as Lex

type Parser = Parsec Void ByteString

pZig :: Parser Zig
pZig = skip *> (Zig <$> pTopLevel) <* eof

pTopLevel :: Parser [TopLevel]
pTopLevel =
  choice
    [ (:) <$> pTestDecl <*> pTopLevel
    , (:) <$> pBlockExpr <*> pTopLevel
    , (:) <$> pTlFunction <*> pTopLevel
    , (:) <$> pContainerField <*> (comma *> pTopLevel)
    , pure <$> pContainerField
    , pure []
    ]
 where
  pTlFunction = TlFunction <$> pVisibility <*> pFunction
  pBlockExpr :: Parser TopLevel
  pBlockExpr = todo "BlockExpr"
  pTestDecl :: Parser TopLevel
  pTestDecl = todo "TestDecl"
  pContainerField :: Parser TopLevel
  pContainerField = todo "ContainerField"

pVisibility :: Parser Visibility
pVisibility = Public <$ keyword "pub" <|> pure Private

pCompileTime :: Parser CompileTime
pCompileTime = CompileTime <$ keyword "comptime" <|> pure Runtime

pConstness :: Parser Constness
pConstness = Const <$ keyword "const" <|> Var <$ keyword "var"

pIdentifier :: Parser Identifier
pIdentifier =
  choice
    [ todo "@\"identifier\""
    , lexeme word >>= \w ->
        if isKeyword w
          then fail $ BS8.unpack w <> " is a keyword"
          else pure $ Identifier w
    ]

pFunction :: Parser Function
pFunction =
  keyword "fn"
    *> ( Function
          <$> pIdentifier
          <*> parens (commaSep pParamDecl)
          <*> optional pAlignment
          <*> optional pLinking
          <*> pReturnType
          <*> (pBlock <|> pure [])
       )

pParamDecl :: Parser ParamDecl
pParamDecl = todo "ParamDecl"

pAlignment :: Parser Alignment
pAlignment = todo "Alignment"

pLinking :: Parser Linking
pLinking = todo "Linking"

pReturnType :: Parser ReturnType
pReturnType = (TypeExpression <$> pTypeExpr) <|> (AnyType <$ lookAhead (keyword "anytype"))

pTypeExpr :: Parser TypeExpression
pTypeExpr = TypeVariable <$> pIdentifier

pBlock :: Parser [Statement]
pBlock = braces (many pStatement)

pStatement :: Parser Statement
pStatement = DeclarationStatement <$> pDeclaration

pDeclaration :: Parser Declaration
pDeclaration =
  Declaration
    <$> pCompileTime
    <*> pConstness
    <*> (Nothing <$ symbol "_" <|> Just <$> pIdentifier)
    <*> optional (colon *> pTypeExpr)
    <*> optional pAlignment
    <*> optional pLinking
    <*> optional (equals *> pExpression)

pExpression :: Parser Expression
pExpression = do
  fs <- many (Try <$ keyword "try")
  a <- pBoolOrExpr
  pure $ foldr ($) a fs

pBoolOrExpr :: Parser Expression
pBoolOrExpr = interspersed pBoolAndExpr (Or <$ keyword "or")

pBoolAndExpr :: Parser Expression
pBoolAndExpr = interspersed pCompareExpr (And <$ keyword "and")

pCompareExpr :: Parser Expression
pCompareExpr = do
  e <- pBitwiseExpr
  x <- optional ((,) <$> pCompareOp <*> pBitwiseExpr)
  case x of
    Nothing -> pure e
    Just (f, e') -> f e e'
 where
  pCompareOp = todo "comparator"

pBitwiseExpr :: Parser Expression
pBitwiseExpr = interspersed pBitShiftExpr pBitwiseOp
 where
  pBitwiseOp = todo "bitwiser"

pBitShiftExpr :: Parser Expression
pBitShiftExpr = interspersed pAdditionExpr pBitShiftOp
 where
  pBitShiftOp = todo "bitshift"

pAdditionExpr :: Parser Expression
pAdditionExpr = interspersed pMultiplyExpr pAdditionOp
 where
  pAdditionOp = todo "addition"

pMultiplyExpr :: Parser Expression
pMultiplyExpr = interspersed pPrefixExpr pMultiplyOp
 where
  pMultiplyOp :: Parser (Expression -> Expression -> Expression)
  pMultiplyOp = Mul <$ asterisk

pPrefixExpr :: Parser Expression
pPrefixExpr = do
  fs <- many pPrefixOp
  a <- pPrimaryExpr
  pure $ foldr ($) a fs
 where
  pPrefixOp :: Parser (Expression -> Expression)
  pPrefixOp = todo "prefixop"

-- comb <$> pa <*> many ((,) <$> pi <*> pa)

-- interspersed :: MonadPlus p => (a -> [(i, a)] -> r) -> p a -> p i -> p r
-- interspersed comb pa pi = comb <$> pa <*> many ((,) <$> pi <*> pa)

pPrimaryExpr :: Parser Expression
pPrimaryExpr =
  choice
    [ todo "asm" -- AsmExpr
    , todo "if" -- IfExpr
    , todo "break" -- KEYWORD_break BreakLabel? Expr?
    , todo "comptime" -- KEYWORD_comptime Expr
    , todo "nosuspend" -- KEYWORD_nosuspend Expr
    , todo "continue" -- KEYWORD_continue BreakLabel?
    , todo "resume" -- KEYWORD_resume Expr
    , Ret <$> (keyword "return" *> optional pExpression) -- KEYWORD_return Expr?
    , todo "block label" -- BlockLabel? LoopExpr
    , todo "block" -- Block
    , pCurlySuffixExpr -- CurlySuffixExpr
    ]

pCurlySuffixExpr :: Parser Expression
pCurlySuffixExpr = todo "curly"

----- Utils

lexeme :: Parser a -> Parser a
lexeme parser = parser <* skip

word :: Parser ByteString
word = do
  h <- satisfy isAlpha_ <?> "alphabetic character"
  t <- takeWhileP (Just "alphanumeric character") isAlphaNum_
  pure $ BS.cons h t

isLower, isUpper, isUnderscore, isDigit, isAlpha_, isAlphaNum_ :: Word8 -> Bool
isLower c = c >= 97 && c <= 122
isUpper c = c >= 65 && c <= 90
isDigit c = c >= 48 && c <= 57
isUnderscore c = c == 95
isAlpha_ c = isLower c || isUpper c || isUnderscore c
isAlphaNum_ c = isLower c || isUpper c || isUnderscore c || isDigit c

keyword :: ByteString -> Parser ()
keyword str = do
  next <- lookAhead $ word -- TODO only parse once
  if next == str
    then lexeme (symbol next)
    else
      failure
        (Just . Label $ '"' :| (BS8.unpack next <> "\""))
        (S.singleton . Label $ '"' :| (BS8.unpack str <> "\""))
-- $ "expected " <> BS8.unpack str <> ", but got " <> BS8.unpack next

todo :: String -> Parser a
todo str = failure Nothing $ S.singleton (Label $ NE.fromList (str <> "(t)"))

symbol :: ByteString -> Parser ()
symbol bs = void $ Lex.symbol skip bs

skip :: Parser ()
skip = Lex.space space1 (Lex.skipLineComment "//") empty

interspersed :: MonadPlus m => m a -> m (a -> a -> a) -> m a
interspersed ma mf = do
  a0 <- ma
  ias <- many $ (,) <$> mf <*> ma
  pure $ foldr (\(f, a') a -> f a a') a0 ias

comma, colon, semicolon, lbrace, rbrace, lparen, rparen, equals :: Parser ()
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
lbrace = symbol "{"
rbrace = symbol "}"
lparen = symbol "("
rparen = symbol ")"
equals = symbol "="
asterisk = symbol "*"

parens :: Parser a -> Parser a
parens = between lparen rparen

braces :: Parser a -> Parser a
braces = between lbrace rbrace

commaSep :: Parser a -> Parser [a]
commaSep = flip sepEndBy comma

isKeyword :: ByteString -> Bool
isKeyword =
  flip S.member $
    S.fromList ["align", "allowzero", "and", "anyframe", "anytype", "asm", "async", "await", "break", "catch", "comptime", "const", "continue", "defer", "else", "enum", "errdefer", "error", "export", "extern", "false", "fn", "for", "if", "inline", "noalias", "nosuspend", "null", "opaque", "or", "orelse", "packed", "pub", "resume", "return", "linksection", "struct", "suspend", "switch", "test", "threadlocal", "true", "try", "undefined", "union", "unreachable", "usingnamespace", "var", "volatile", "while"]
