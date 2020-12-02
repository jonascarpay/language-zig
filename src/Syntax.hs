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

-- TODO Inline everything that's not used more than once

pZig :: Parser StructDef
pZig = do
  skip
  members <- pContainerMembers <* eof
  either (fmap absurd) pure $ mkStructDef Nothing Nothing members

pContainerMembers :: Parser [ContainerMember]
pContainerMembers =
  choice
    [ (:) <$> pTestDecl <*> pContainerMembers
    , (:) <$> pBlockExpr <*> pContainerMembers
    , (:) <$> pTlFunction <*> pContainerMembers
    , (:) <$> pContainerField <*> (comma *> pContainerMembers)
    , pure <$> pContainerField
    , pure []
    ]
 where
  pTlFunction = TlFunction <$> pVisibility <*> pFunction
  pBlockExpr :: Parser ContainerMember
  pBlockExpr = todo "BlockExpr"
  pTestDecl :: Parser ContainerMember
  pTestDecl = todo "TestDecl"
  pContainerField :: Parser ContainerMember
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
    , wordSuchThat $ \w ->
        if isKeyword w
          then Left $ unexpected $ Label $ 'k' :| ("eyword \"" <> BS8.unpack w <> "\"")
          else Right $ Identifier w
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
pReturnType = (TypeExpression <$> pTypeExpr) <|> (AnyType <$ keyword "anytype")

pTypeExpr :: Parser Expression
pTypeExpr = do
  prefixes <- many pPrefixTypeOp
  expr <- pErrorUnionExpr
  pure $ foldr (.) id prefixes expr

pPrefixTypeOp :: Parser (Expression -> Expression)
pPrefixTypeOp =
  choice
    [ Questionmark <$ questionmark
    , todo "AnyFrame"
    , todo "ArrayTypeStart"
    , todo "PtrTypeStart"
    ]

pErrorUnionExpr :: Parser Expression
pErrorUnionExpr = do
  suffix <- pSuffixExpr
  mtail <- optional (exclamation *> pTypeExpr)
  pure $ case mtail of
    Nothing -> suffix
    Just err -> ErrorUnion suffix err

pSuffixExpr :: Parser Expression
pSuffixExpr = pAsync <|> pNonAsync
 where
  pAsync :: Parser Expression
  pAsync = todo "async"
  pNonAsync :: Parser Expression
  pNonAsync = do
    prim <- pPrimaryTypeExpr
    _ <- many (todo "suffix")
    pure prim

pPrimaryTypeExpr :: Parser Expression
pPrimaryTypeExpr =
  choice
    [ todo "PrimBuiltin" -- PrimBuiltin (BuiltinIdentifier a) (FnCallArguments a)
    , todo "PrimCharLiteral" -- PrimCharLiteral (CharLiteral a)
    , pContainerDecl -- PrimContainer (ContainerDecl a)
    , todo "PrimDotId" -- PrimDotId (Identifier a)
    , todo "PrimInitList" -- PrimInitList (InitList a)
    , todo "PrimErrorSet" -- PrimErrorSet (ErrorSetDecl a)
    , todo "PrimFloat" -- PrimFloat (FloatLit a)
    , todo "PrimFnProto" -- PrimFnProto (FnProto a)
    , todo "PrimGrouped" -- PrimGrouped (GroupedExpr a)
    , todo "PrimLabeledType" -- PrimLabeledType (LabeledTypeExpr a)
    , IdentifierExpr <$> pIdentifier -- PrimId (Identifier a)
    , todo "PrimIfType" -- PrimIfType (IfTypeExpr a)
    , Int <$> pIntLit -- PrimInt (IntLit a)
    , todo "PrimType" -- PrimType (KeywordComptime a) (TypeExpr a)
    , todo "PrimErrId" -- PrimErrId (KeywordError a) (Identifier a)
    , todo "PrimFalse" -- PrimFalse (KeywordFalse a)
    , todo "PrimNull" -- PrimNull (KeywordNull a)
    , todo "PrimAnyFrame" -- PrimAnyFrame (KeywordAnyframe a)
    , todo "PrimTrue" -- PrimTrue (KeywordTrue a)
    , todo "PrimUndefined" -- PrimUndefined (KeywordUndefined a)
    , todo "PrimUnreachable" -- PrimUnreachable (KeywordUnreachable a)
    , todo "PrimString" -- PrimString (StringLit a)
    , todo "PrimSwitch" -- PrimSwitch (SwitchExpr a)
    ]
 where
  pContainerDecl :: Parser Expression
  pContainerDecl = do
    q <- optional (todo "Container Qualifier")
    t <- pContainerDeclType
    ms <- braces pContainerMembers
    either (fmap absurd) pure $ mkContainer q t ms
  pContainerDeclType :: Parser ContainerType
  pContainerDeclType =
    choice
      [ keyword "struct" *> (Struct <$> optional (parens pExpr))
      , todo "ContEnum" -- ContEnum (KeywordEnum a) (Maybe (Expr a))
      , todo "ContOpaque" -- ContOpaque (KeywordOpaque a) (Maybe (Expr a))
      , todo "ContEmptyUnion" -- ContEmptyUnion (KeywordUnion a)
      , todo "ContEnumUnion" -- ContEnumUnion (KeywordUnion a) (KeywordEnum a) (Maybe (Expr a))
      , todo "ContUnion" -- ContUnion (KeywordUnion a) (Expr a)
      ]
  mkContainer :: Maybe ContainerQualifier -> ContainerType -> [ContainerMember] -> Either (Parser Void) Expression
  mkContainer q (Struct expr) mems = StructDefExpr <$> mkStructDef q expr mems
  mkContainer _ _ _ = Left $ fail "invalid container definition"

mkStructDef :: Maybe ContainerQualifier -> Maybe Expression -> [ContainerMember] -> Either (Parser Void) StructDef
mkStructDef q Nothing ms = pure $ StructDef q

pIntLit :: Parser Integer
pIntLit = lexeme Lex.decimal

pBlock :: Parser [Statement]
pBlock = braces (many pStatement)

pStatement :: Parser Statement
pStatement =
  choice
    [ DeclarationStatement <$> pDeclaration
    , todo "StmtComptime" -- StmtComptime (KeywordComptime a) (BlockExprStatement a)
    , todo "StmtNoSuspend" -- StmtNoSuspend (KeywordNoSuspend a) (BlockExprStatement a)
    , todo "StmtSuspend" -- StmtSuspend (KeywordSuspend a) (Maybe (BlockExprStatement a))
    , todo "StmtDefer" -- StmtDefer (KeywordDefer a) (BlockExprStatement a)
    , todo "StmtErrDefer" -- StmtErrDefer (KeywordErrdefer a) (BlockExprStatement a)
    , todo "StmtIf" -- StmtIf (IfStatement a)
    , todo "StmtLabeled" -- StmtLabeled (LabeledStatement a)
    , todo "StmtSwitch" -- StmtSwitch (SwitchExpr a)
    , pAssignExpr <* semicolon
    ]
 where
  pAssignExpr :: Parser Statement
  pAssignExpr = do
    lead <- pExpr
    mtail <- optional (todo "AssignOp Expr")
    pure $ case mtail of
      Nothing -> ExpressionStatement lead
      Just (op, val) -> AssignmentStatement lead op val

pDeclaration :: Parser Declaration
pDeclaration =
  Declaration
    <$> pCompileTime
    <*> pConstness
    <*> (Nothing <$ symbol "_" <|> Just <$> pIdentifier)
    <*> optional (colon *> pTypeExpr)
    <*> optional pAlignment
    <*> optional pLinking
    <*> optional (equals *> pExpr) -- TODO not actually optional?
    <* semicolon

pExpr :: Parser Expression
pExpr = do
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
    , Ret <$> (keyword "return" *> optional pExpr) -- KEYWORD_return Expr?
    , todo "block label" -- BlockLabel? LoopExpr
    , todo "block" -- Block
    , pCurlySuffixExpr -- CurlySuffixExpr
    ]

pCurlySuffixExpr :: Parser Expression
pCurlySuffixExpr = do
  expr <- pTypeExpr
  mInitList <- optional pInitList
  pure $ case mInitList of
    Nothing -> expr
    Just initList -> CurlySuffix expr initList

pInitList :: Parser InitList
pInitList = todo "InitList"

----- Utils

lexeme :: Parser a -> Parser a
lexeme parser = parser <* skip

word :: Parser ByteString
word = do
  h <- satisfy isAlpha_ <?> "alphabetic character"
  t <- takeWhileP (Just "alphanumeric character") isAlphaNum_
  pure $ BS.cons h t

wordSuchThat :: (ByteString -> Either (Parser Void) a) -> Parser a
wordSuchThat f = do
  w <- lookAhead word
  case f w of
    Right a -> a <$ lexeme (chunk w)
    Left err -> (absurd <$> err)

-- Does not work for some reason
-- wordSuchThat :: (ByteString -> Either (Parser Void) a) -> Parser a
-- wordSuchThat f = do
--   pre <- getOffset
--   w <- lexeme word
--   case f w of
--     Right a -> a <$ skip
--     Left err -> setOffset pre >> (absurd <$> err)

isLower, isUpper, isUnderscore, isDigit, isAlpha_, isAlphaNum_ :: Word8 -> Bool
isLower c = c >= 97 && c <= 122
isUpper c = c >= 65 && c <= 90
isDigit c = c >= 48 && c <= 57
isUnderscore c = c == 95
isAlpha_ c = isLower c || isUpper c || isUnderscore c
isAlphaNum_ c = isLower c || isUpper c || isUnderscore c || isDigit c

keyword :: ByteString -> Parser ()
keyword str = wordSuchThat $ \w ->
  if str == w
    then pure ()
    else
      Left $
        failure
          (Just . Label $ '"' :| (BS8.unpack w <> "\""))
          (S.singleton . Label $ '"' :| (BS8.unpack str <> "\""))

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

comma, colon, semicolon, lbrace, rbrace, lparen, rparen, equals, asterisk, questionmark, exclamation :: Parser ()
comma = symbol ","
colon = symbol ":"
semicolon = symbol ";"
lbrace = symbol "{"
rbrace = symbol "}"
lparen = symbol "("
rparen = symbol ")"
equals = symbol "="
asterisk = symbol "*"
questionmark = symbol "?"
exclamation = symbol "!"

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
