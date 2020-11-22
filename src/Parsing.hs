{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Control.Applicative hiding (many)
import Control.Applicative.Combinators.NonEmpty qualified as NE
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Set qualified as S
import Grammar
import Parser
import Text.Megaparsec

pIdentifier :: Parser (Identifier Span)
pIdentifier = do
  atId <- True <$ chunk "@" <|> pure False
  (s, w) <- lexeme word
  when (isKeyword w) $ fail $ BS8.unpack w <> " is a keyword" -- TODO proper error
  pure $ if atId then Identifier s w else AtIdentifier s w

pTodo :: Parser a
pTodo = fail "todo"

-- Root <- skip ContainerMembers eof

pRoot :: Parser (Root Span)
pRoot = skip *> (Root <$> pContainerMembers)

-- ContainerMembers
--     <- TestDecl ContainerMembers
--      / TopLevelComptime ContainerMembers
--      / KEYWORD_pub? TopLevelDecl ContainerMembers
--      / ContainerField COMMA ContainerMembers
--      / ContainerField
--      /

pContainerMembers :: Parser (ContainerMembers Span)
pContainerMembers =
  choice
    [ liftA2 CMTestDecl pTestDecl pContainerMembers
    , liftA2 CMTopLevelComptime pTopLevelComptime pContainerMembers
    , liftA3 CMTopLevelDecl (optional pKeywordPub) pTopLevelDecl pContainerMembers
    , liftA2 CMContainerField pContainerField (optional pContainerMembers)
    ]

-- TestDecl <- KEYWORD_test STRINGLITERALSINGLE Block

pTestDecl :: Parser (TestDecl Span)
pTestDecl = fail "TestDecl"

-- TopLevelComptime <- KEYWORD_comptime BlockExpr

pTopLevelComptime :: Parser (TopLevelComptime Span)
pTopLevelComptime = fail "TopLevelComptime"

-- TopLevelDecl
--     <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / KEYWORD_inline)? FnProto (SEMICOLON / Block)
--      / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? VarDecl
--      / KEYWORD_usingnamespace Expr SEMICOLON

pTopLevelDecl :: Parser (TopLevelDecl Span)
pTopLevelDecl =
  choice
    [ liftA3 TLFnProto (optional pTlFnProtoQualifier) pFnProto (Just <$> pBlock <|> Nothing <$ semicolon)
    , liftA3 TLVarDecl (optional pTodo) (optional pKeywordThreadlocal) pVarDecl
    , liftA2 TLUsingNamespace pKeywordUsingnamespace pExpr
    ]
 where
  -- TODO properly branch on intial keyword
  pTlFnProtoQualifier :: Parser (TLFnProtoQualifier Span)
  pTlFnProtoQualifier =
    choice
      [ FnProtoExport <$> pKeywordExport
      , FnProtoExtern <$> pKeywordExtern <*> optional pStringLiteralSingle
      , FnProtoInline <$> pKeywordInline
      ]

-- FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? LinkSection? EXCLAMATIONMARK? (KEYWORD_anytype / TypeExpr)

pFnProto :: Parser (FnProto Span)
pFnProto =
  FnProto
    <$> pKeywordFn
    <*> optional pIdentifier
    <*> parens pParamDeclList
    <*> optional (fail "byteAlign")
    <*> optional (fail "linksection")
    <*> optional (symbol "!")
    <*> (pKeywordAnytype <+> pTypeExpr)

-- VarDecl <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? LinkSection? (EQUAL Expr)? SEMICOLON

pVarDecl :: Parser (VarDecl Span)
pVarDecl = fail "VarDecl"

-- ContainerField <- KEYWORD_comptime? IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?

pContainerField :: Parser (ContainerField Span)
pContainerField = fail "ContainerField"

-- Statement

pStatement :: Parser (Statement Span)
pStatement =
  choice
    [ pTodo -- KEYWORD_comptime? VarDecl
    , pTodo -- KEYWORD_comptime BlockExprStatement
    , pTodo -- KEYWORD_nosuspend BlockExprStatement
    , pTodo -- KEYWORD_suspend (SEMICOLON / BlockExprStatement)
    , pTodo -- KEYWORD_defer BlockExprStatement
    , pTodo -- KEYWORD_errdefer BlockExprStatement
    , pTodo -- IfStatement
    , pTodo -- LabeledStatement
    , pTodo -- SwitchExpr
    , StmtAssign <$> pAssignExpr -- AssignExpr SEMICOLON
    ]

-- IfStatement
--     <- IfPrefix BlockExpr ( KEYWORD_else Payload? Statement )?
--      / IfPrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
--
-- LabeledStatement <- BlockLabel? (Block / LoopStatement)
--
-- LoopStatement <- KEYWORD_inline? (ForStatement / WhileStatement)
--
-- ForStatement
--     <- ForPrefix BlockExpr ( KEYWORD_else Statement )?
--      / ForPrefix AssignExpr ( SEMICOLON / KEYWORD_else Statement )
--
-- WhileStatement
--     <- WhilePrefix BlockExpr ( KEYWORD_else Payload? Statement )?
--      / WhilePrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
--
-- BlockExprStatement
--     <- BlockExpr
--      / AssignExpr SEMICOLON
--
-- BlockExpr <- BlockLabel? Block

pAssignExpr :: Parser (AssignExpr Span)
pAssignExpr = AssignExpr <$> pExpr <*> (optional $ (,) <$> pAssignOp <*> pExpr)

pExpr :: Parser (Expr Span)
pExpr = Expr <$> many pKeywordTry <*> pBoolOrExpr

pBoolOrExpr :: Parser (BoolOrExpr Span)
pBoolOrExpr = BoolOrExpr <$> pBoolAndExpr `NE.sepBy1` pKeywordOr

pBoolAndExpr :: Parser (BoolAndExpr Span)
pBoolAndExpr = BoolAndExpr <$> pCompareExpr `NE.sepBy1` pKeywordAnd

pCompareExpr :: Parser (CompareExpr Span)
pCompareExpr = CompareExpr <$> pBitwiseExpr <*> optional ((,) <$> pCompareOp <*> pBitwiseExpr)

pBitwiseExpr :: Parser (BitwiseExpr Span)
pBitwiseExpr = interspersed BitwiseExpr pBitShiftExpr pBitwiseOp

pBitShiftExpr :: Parser (BitShiftExpr Span)
pBitShiftExpr = interspersed BitShiftExpr pAdditionExpr pBitShiftOp

pAdditionExpr :: Parser (AdditionExpr Span)
pAdditionExpr = interspersed AdditionExpr pMultiplyExpr pAdditionOp

pMultiplyExpr :: Parser (MultiplyExpr Span)
pMultiplyExpr = interspersed MultiplyExpr pPrefixExpr pMultiplyOp

pPrefixExpr :: Parser (PrefixExpr Span)
pPrefixExpr = PrefixExpr <$> many pPrefixOp <*> pPrimaryExpr

pPrimaryExpr :: Parser (PrimaryExpr Span)
pPrimaryExpr =
  choice
    [ pTodo -- AsmExpr
    , pTodo -- IfExpr
    , pTodo -- KEYWORD_break BreakLabel? Expr?
    , pTodo -- KEYWORD_comptime Expr
    , pTodo -- KEYWORD_nosuspend Expr
    , pTodo -- KEYWORD_continue BreakLabel?
    , pTodo -- KEYWORD_resume Expr
    , pTodo -- KEYWORD_return Expr?
    , pTodo -- BlockLabel? LoopExpr
    , pTodo -- Block
    , pTodo -- CurlySuffixExpr
    ]

-- IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?

pBlock :: Parser (Block Span)
pBlock = Block <$> (braces $ many pStatement)

-- LoopExpr <- KEYWORD_inline? (ForExpr / WhileExpr)
--
-- ForExpr <- ForPrefix Expr (KEYWORD_else Expr)?
--
-- WhileExpr <- WhilePrefix Expr (KEYWORD_else Payload? Expr)?
--
-- CurlySuffixExpr <- TypeExpr InitList?
--
-- InitList
--     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
--      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
--      / LBRACE RBRACE
--
-- TypeExpr <- PrefixTypeOp* ErrorUnionExpr

pTypeExpr :: Parser (TypeExpr Span)
pTypeExpr = TypeExpr <$> many pPrefixTypeOp <*> pErrorUnionExpr

-- ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?

pErrorUnionExpr :: Parser (ErrorUnionExpr Span)
pErrorUnionExpr = ErrorUnionExpr <$> pSuffixExpr <*> optional pTypeExpr

-- SuffixExpr
--     <- KEYWORD_async PrimaryTypeExpr SuffixOp* FnCallArguments
--      / PrimaryTypeExpr (SuffixOp / FnCallArguments)*

pSuffixExpr :: Parser (SuffixExpr Span)
pSuffixExpr =
  choice
    [ SuffixAsync
        <$> pKeywordAsync
        <*> pPrimaryTypeExpr
        <*> many pSuffixOp
        <*> pFnCallArguments
    , SuffixExpr
        <$> pPrimaryTypeExpr
        <*> many (pSuffixOp <+> pFnCallArguments)
    ]

-- PrimaryTypeExpr
--     <- BUILTINIDENTIFIER FnCallArguments
--      / CHAR_LITERAL
--      / ContainerDecl
--      / DOT IDENTIFIER
--      / DOT InitList
--      / ErrorSetDecl
--      / FLOAT
--      / FnProto
--      / GroupedExpr
--      / LabeledTypeExpr
--      / IDENTIFIER
--      / IfTypeExpr
--      / INTEGER
--      / KEYWORD_comptime TypeExpr
--      / KEYWORD_error DOT IDENTIFIER
--      / KEYWORD_false
--      / KEYWORD_null
--      / KEYWORD_anyframe
--      / KEYWORD_true
--      / KEYWORD_undefined
--      / KEYWORD_unreachable
--      / STRINGLITERAL
--      / SwitchExpr

pPrimaryTypeExpr :: Parser (PrimaryTypeExpr Span)
pPrimaryTypeExpr =
  choice
    [ pTodo --  BUILTINIDENTIFIER FnCallArguments
    , pTodo --  CHAR_LITERAL
    , pTodo --  ContainerDecl
    , pTodo -- PrimDotId <$> (symbol "." *> pIdentifier) --  DOT IDENTIFIER
    , pTodo --  DOT InitList
    , pTodo --  ErrorSetDecl
    , pTodo --  FLOAT
    , pTodo -- PrimFnProto <$> pFnProto --  FnProto
    , pTodo --  GroupedExpr
    , pTodo --  LabeledTypeExpr
    , PrimId <$> pIdentifier --  IDENTIFIER
    , pTodo --  IfTypeExpr
    , pTodo --  INTEGER
    , pTodo --  KEYWORD_comptime TypeExpr
    , pTodo -- PrimErrId <$> pKeywordError <*> (symbol "." *> pIdentifier) --  KEYWORD_error DOT IDENTIFIER
    , pTodo -- PrimFalse <$> pKeywordFalse --  KEYWORD_false
    , pTodo -- PrimNull <$> pKeywordNull --  KEYWORD_null
    , pTodo -- PrimAnyFrame <$> pKeywordAnyframe --  KEYWORD_anyframe
    , pTodo -- PrimTrue <$> pKeywordTrue --  KEYWORD_true
    , pTodo -- PrimUndefined <$> pKeywordUndefined --  KEYWORD_undefined
    , pTodo -- PrimUnreachable <$> pKeywordUnreachable --  KEYWORD_unreachable
    , pTodo --  STRINGLITERAL
    , pTodo --  SwitchExpr
    ]

-- ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto
--
-- ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE
--
-- GroupedExpr <- LPAREN Expr RPAREN
--
-- IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
--
-- LabeledTypeExpr
--     <- BlockLabel Block
--      / BlockLabel? LoopTypeExpr
--
-- LoopTypeExpr <- KEYWORD_inline? (ForTypeExpr / WhileTypeExpr)
--
-- ForTypeExpr <- ForPrefix TypeExpr (KEYWORD_else TypeExpr)?
--
-- WhileTypeExpr <- WhilePrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
--
-- SwitchExpr <- KEYWORD_switch LPAREN Expr RPAREN LBRACE SwitchProngList RBRACE
--
-- # *** Assembly ***
-- AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN STRINGLITERAL AsmOutput? RPAREN
--
-- AsmOutput <- COLON AsmOutputList AsmInput?
--
-- AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN
--
-- AsmInput <- COLON AsmInputList AsmClobbers?
--
-- AsmInputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN Expr RPAREN
--
-- AsmClobbers <- COLON StringList
--
-- # *** Helper grammar ***
-- BreakLabel <- COLON IDENTIFIER
--
-- BlockLabel <- IDENTIFIER COLON
--
-- FieldInit <- DOT IDENTIFIER EQUAL Expr
--
-- WhileContinueExpr <- COLON LPAREN AssignExpr RPAREN
--
-- LinkSection <- KEYWORD_linksection LPAREN Expr RPAREN
--
-- ParamDecl <- (KEYWORD_noalias / KEYWORD_comptime)? (IDENTIFIER COLON)? ParamType

pParamDecl :: Parser (ParamDecl Span)
pParamDecl = fail "ParamDecl"

-- ParamType
--     <- KEYWORD_anytype
--      / DOT3
--      / TypeExpr
--
-- # Control flow prefixes
-- IfPrefix <- KEYWORD_if LPAREN Expr RPAREN PtrPayload?
--
-- WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?
--
-- ForPrefix <- KEYWORD_for LPAREN Expr RPAREN PtrIndexPayload
--
-- # Payloads
-- Payload <- PIPE IDENTIFIER PIPE
--
-- PtrPayload <- PIPE ASTERISK? IDENTIFIER PIPE
--
-- PtrIndexPayload <- PIPE ASTERISK? IDENTIFIER (COMMA IDENTIFIER)? PIPE
--
--
-- # Switch specific
-- SwitchProng <- SwitchCase EQUALRARROW PtrPayload? AssignExpr
--
-- SwitchCase
--     <- SwitchItem (COMMA SwitchItem)* COMMA?
--      / KEYWORD_else
--
-- SwitchItem <- Expr (DOT3 Expr)?
--
-- # Operators
-- AssignOp
--     <- ASTERISKEQUAL
--      / SLASHEQUAL
--      / PERCENTEQUAL
--      / PLUSEQUAL
--      / MINUSEQUAL
--      / LARROW2EQUAL
--      / RARROW2EQUAL
--      / AMPERSANDEQUAL
--      / CARETEQUAL
--      / PIPEEQUAL
--      / ASTERISKPERCENTEQUAL
--      / PLUSPERCENTEQUAL
--      / MINUSPERCENTEQUAL
--      / EQUAL

pAssignOp :: Parser (AssignOp Span)
pAssignOp = fail "AssignOp"

pCompareOp :: Parser (CompareOp Span)
pCompareOp =
  choice
    [ CompareEqual <$> symbol "==" -- EQUALEQUAL
    , CompareNotEqual <$> symbol "!=" -- EXCLAMATIONMARKEQUAL
    , pTodo -- LARROW
    , pTodo -- RARROW
    , pTodo -- LARROWEQUAL
    , pTodo -- RARROWEQUAL
    ]

pBitwiseOp :: Parser (BitwiseOp Span)
pBitwiseOp =
  choice
    [ pTodo -- AMPERSAND
    , pTodo --  CARET
    , pTodo --  PIPE
    , pTodo --  KEYWORD_orelse
    , pTodo --  KEYWORD_catch Payload?
    ]

pBitShiftOp :: Parser (BitShiftOp Span)
pBitShiftOp =
  choice
    [ BitShiftLarrow2 <$> symbol "<<"
    , BitShiftRarrow2 <$> symbol ">>"
    ]

pAdditionOp :: Parser (AdditionOp Span)
pAdditionOp =
  choice
    [ pTodo -- PLUS
    , pTodo -- MINUS
    , pTodo -- PLUS2
    , pTodo -- PLUSPERCENT
    , pTodo -- MINUSPERCENT
    ]

pMultiplyOp :: Parser (MultiplyOp Span)
pMultiplyOp =
  choice
    [ pTodo -- PIPE2
    , pTodo -- ASTERISK
    , pTodo -- SLASH
    , pTodo -- PERCENT
    , pTodo -- ASTERISK2
    , pTodo -- ASTERISKPERCENT
    ]

pPrefixOp :: Parser (PrefixOp Span)
pPrefixOp =
  choice
    [ pTodo -- EXCLAMATIONMARK
    , pTodo -- MINUS
    , pTodo -- TILDE
    , pTodo -- MINUSPERCENT
    , pTodo -- AMPERSAND
    , pTodo -- KEYWORD_try
    , pTodo -- KEYWORD_await
    ]

--
-- PrefixTypeOp
--     <- QUESTIONMARK
--      / KEYWORD_anyframe MINUSRARROW
--      / ArrayTypeStart (ByteAlign / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
--      / PtrTypeStart (KEYWORD_align LPAREN Expr (COLON INTEGER COLON INTEGER)? RPAREN / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*

pPrefixTypeOp :: Parser (PrefixTypeOp Span)
pPrefixTypeOp =
  choice
    [ PrefixTypeQuestion <$> symbol "?"
    , PrefixTypeAnyFrame <$> pKeywordAnyframe
    , PrefixTypeArrayTypeStart <$> fail "PrefixTypeArrayTypeStart"
    , PrefixTypePtrTypeStart <$> fail "PrefixTypePtrTypeStart"
    ]

-- SuffixOp
--     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
--      / DOT IDENTIFIER
--      / DOTASTERISK
--      / DOTQUESTIONMARK

pSuffixOp :: Parser (SuffixOp Span)
pSuffixOp =
  choice
    []

-- FnCallArguments <- LPAREN ExprList RPAREN

pFnCallArguments :: Parser (FnCallArguments Span)
pFnCallArguments = fail "FnCallArguments"

-- # Ptr specific
-- ArrayTypeStart <- LBRACKET Expr? RBRACKET
--
-- PtrTypeStart
--     <- ASTERISK
--      / ASTERISK2
--      / PTRUNKNOWN
--      / PTRC
--
-- # ContainerDecl specific
-- ContainerDeclAuto <- ContainerDeclType LBRACE ContainerMembers RBRACE
--
-- ContainerDeclType
--     <- (KEYWORD_struct / KEYWORD_enum / KEYWORD_opaque) (LPAREN Expr RPAREN)?
--      / KEYWORD_union (LPAREN (KEYWORD_enum (LPAREN Expr RPAREN)? / Expr) RPAREN)?
--
-- # Alignment
-- ByteAlign <- KEYWORD_align LPAREN Expr RPAREN
--
-- # Lists
-- IdentifierList <- (IDENTIFIER COMMA)* IDENTIFIER?
--
-- SwitchProngList <- (SwitchProng COMMA)* SwitchProng?
--
-- AsmOutputList <- (AsmOutputItem COMMA)* AsmOutputItem?
--
-- AsmInputList <- (AsmInputItem COMMA)* AsmInputItem?
--
-- StringList <- (STRINGLITERAL COMMA)* STRINGLITERAL?
--
-- ParamDeclList <- (ParamDecl COMMA)* ParamDecl?

pParamDeclList :: Parser [ParamDecl Span]
pParamDeclList = commaSep pParamDecl

-- ExprList <- (Expr COMMA)* Expr?
--
-- # *** Tokens ***
-- eof <- !.
-- hex <- [0-9a-fA-F]
-- char_escape
--     <- "\\x" hex hex
--      / "\\u{" hex+ "}"
--      / "\\" [nr\\t'"]
-- char_char
--     <- char_escape
--      / [^\\'\n]
-- string_char
--     <- char_escape
--      / [^\\"\n]
--
-- line_comment <- '//'[^\n]*
-- line_string <- ("\\\\" [^\n]* [ \n]*)+
-- skip <- ([ \n] / line_comment)*
--
-- CHAR_LITERAL <- "'" char_char "'" skip
-- FLOAT
--     <- "0x" hex+   "." hex+   ([pP] [-+]? hex+)?   skip
--      /      [0-9]+ "." [0-9]+ ([eE] [-+]? [0-9]+)? skip
--      / "0x" hex+   "."? [pP] [-+]? hex+   skip
--      /      [0-9]+ "."? [eE] [-+]? [0-9]+ skip
-- INTEGER
--     <- "0b" [01]+  skip
--      / "0o" [0-7]+ skip
--      / "0x" hex+   skip
--      /      [0-9]+ skip
-- STRINGLITERALSINGLE <- "\"" string_char* "\"" skip

pStringLiteralSingle :: Parser (StringLiteralSingle Span)
pStringLiteralSingle = fail "StringLiteralSingle"

-- STRINGLITERAL
--     <- STRINGLITERALSINGLE
--      / line_string                 skip
-- IDENTIFIER
--     <- !keyword [A-Za-z_] [A-Za-z0-9_]* skip
--      / "@\"" string_char* "\""                            skip
-- BUILTINIDENTIFIER <- "@"[A-Za-z_][A-Za-z0-9_]* skip
--
--
-- AMPERSAND            <- '&'      ![=]      skip
-- AMPERSANDEQUAL       <- '&='               skip
-- ASTERISK             <- '*'      ![*%=]    skip
-- ASTERISK2            <- '**'               skip
-- ASTERISKEQUAL        <- '*='               skip
-- ASTERISKPERCENT      <- '*%'     ![=]      skip
-- ASTERISKPERCENTEQUAL <- '*%='              skip
-- CARET                <- '^'      ![=]      skip
-- CARETEQUAL           <- '^='               skip
-- COLON                <- ':'                skip
-- COMMA                <- ','                skip
-- DOT                  <- '.'      ![*.?]    skip
-- DOT2                 <- '..'     ![.]      skip
-- DOT3                 <- '...'              skip
-- DOTASTERISK          <- '.*'               skip
-- DOTQUESTIONMARK      <- '.?'               skip
-- EQUAL                <- '='      ![>=]     skip
-- EQUALEQUAL           <- '=='               skip
-- EQUALRARROW          <- '=>'               skip
-- EXCLAMATIONMARK      <- '!'      ![=]      skip
-- EXCLAMATIONMARKEQUAL <- '!='               skip
-- LARROW               <- '<'      ![<=]     skip
-- LARROW2              <- '<<'     ![=]      skip
-- LARROW2EQUAL         <- '<<='              skip
-- LARROWEQUAL          <- '<='               skip
-- LBRACE               <- '{'                skip
-- LBRACKET             <- '['      ![*]      skip
-- LPAREN               <- '('                skip
-- MINUS                <- '-'      ![%=>]    skip
-- MINUSEQUAL           <- '-='               skip
-- MINUSPERCENT         <- '-%'     ![=]      skip
-- MINUSPERCENTEQUAL    <- '-%='              skip
-- MINUSRARROW          <- '->'               skip
-- PERCENT              <- '%'      ![=]      skip
-- PERCENTEQUAL         <- '%='               skip
-- PIPE                 <- '|'      ![|=]     skip
-- PIPE2                <- '||'               skip
-- PIPEEQUAL            <- '|='               skip
-- PLUS                 <- '+'      ![%+=]    skip
-- PLUS2                <- '++'               skip
-- PLUSEQUAL            <- '+='               skip
-- PLUSPERCENT          <- '+%'     ![=]      skip
-- PLUSPERCENTEQUAL     <- '+%='              skip
-- PTRC                 <- '[*c]'             skip
-- PTRUNKNOWN           <- '[*]'              skip
-- QUESTIONMARK         <- '?'                skip
-- RARROW               <- '>'      ![>=]     skip
-- RARROW2              <- '>>'     ![=]      skip
-- RARROW2EQUAL         <- '>>='              skip
-- RARROWEQUAL          <- '>='               skip
-- RBRACE               <- '}'                skip
-- RBRACKET             <- ']'                skip
-- RPAREN               <- ')'                skip
-- SEMICOLON            <- ';'                skip
-- SLASH                <- '/'      ![=]      skip
-- SLASHEQUAL           <- '/='               skip
-- TILDE                <- '~'                skip

isKeyword :: ByteString -> Bool
isKeyword =
  flip S.member $
    S.fromList
      [ "align"
      , "allowzero"
      , "and"
      , "anyframe"
      , "anytype"
      , "asm"
      , "async"
      , "await"
      , "break"
      , "catch"
      , "comptime"
      , "const"
      , "continue"
      , "defer"
      , "else"
      , "enum"
      , "errdefer"
      , "error"
      , "export"
      , "extern"
      , "false"
      , "fn"
      , "for"
      , "if"
      , "inline"
      , "noalias"
      , "nosuspend"
      , "null"
      , "opaque"
      , "or"
      , "orelse"
      , "packed"
      , "pub"
      , "resume"
      , "return"
      , "linksection"
      , "struct"
      , "suspend"
      , "switch"
      , "test"
      , "threadlocal"
      , "true"
      , "try"
      , "undefined"
      , "union"
      , "unreachable"
      , "usingnamespace"
      , "var"
      , "volatile"
      , "while"
      ]
pKeywordAlign :: Parser (KeywordAlign Span)
pKeywordAlign = KeywordAlign <$> keyword "align"

pKeywordAnd :: Parser (KeywordAnd Span)
pKeywordAnd = KeywordAnd <$> keyword "and"

pKeywordAnyframe :: Parser (KeywordAnyframe Span)
pKeywordAnyframe = KeywordAnyframe <$> keyword "anyframe"

pKeywordAnytype :: Parser (KeywordAnytype Span)
pKeywordAnytype = KeywordAnytype <$> keyword "anytype"

pKeywordAllowzero :: Parser (KeywordAllowzero Span)
pKeywordAllowzero = KeywordAllowzero <$> keyword "allowzero"

pKeywordAsm :: Parser (KeywordAsm Span)
pKeywordAsm = KeywordAsm <$> keyword "asm"

pKeywordAsync :: Parser (KeywordAsync Span)
pKeywordAsync = KeywordAsync <$> keyword "async"

pKeywordAwait :: Parser (KeywordAwait Span)
pKeywordAwait = KeywordAwait <$> keyword "await"

pKeywordBreak :: Parser (KeywordBreak Span)
pKeywordBreak = KeywordBreak <$> keyword "break"

pKeywordCatch :: Parser (KeywordCatch Span)
pKeywordCatch = KeywordCatch <$> keyword "catch"

pKeywordComptime :: Parser (KeywordComptime Span)
pKeywordComptime = KeywordComptime <$> keyword "comptime"

pKeywordConst :: Parser (KeywordConst Span)
pKeywordConst = KeywordConst <$> keyword "const"

pKeywordContinue :: Parser (KeywordContinue Span)
pKeywordContinue = KeywordContinue <$> keyword "continue"

pKeywordDefer :: Parser (KeywordDefer Span)
pKeywordDefer = KeywordDefer <$> keyword "defer"

pKeywordElse :: Parser (KeywordElse Span)
pKeywordElse = KeywordElse <$> keyword "else"

pKeywordEnum :: Parser (KeywordEnum Span)
pKeywordEnum = KeywordEnum <$> keyword "enum"

pKeywordErrdefer :: Parser (KeywordErrdefer Span)
pKeywordErrdefer = KeywordErrdefer <$> keyword "errdefer"

pKeywordError :: Parser (KeywordError Span)
pKeywordError = KeywordError <$> keyword "error"

pKeywordExport :: Parser (KeywordExport Span)
pKeywordExport = KeywordExport <$> keyword "export"

pKeywordExtern :: Parser (KeywordExtern Span)
pKeywordExtern = KeywordExtern <$> keyword "extern"

pKeywordFalse :: Parser (KeywordFalse Span)
pKeywordFalse = KeywordFalse <$> keyword "false"

pKeywordFor :: Parser (KeywordFor Span)
pKeywordFor = KeywordFor <$> keyword "for"

pKeywordFn :: Parser (KeywordFn Span)
pKeywordFn = KeywordFn <$> keyword "fn"

pKeywordIf :: Parser (KeywordIf Span)
pKeywordIf = KeywordIf <$> keyword "if"

pKeywordInline :: Parser (KeywordInline Span)
pKeywordInline = KeywordInline <$> keyword "inline"

pKeywordNoalias :: Parser (KeywordNoalias Span)
pKeywordNoalias = KeywordNoalias <$> keyword "noalias"

pKeywordNull :: Parser (KeywordNull Span)
pKeywordNull = KeywordNull <$> keyword "null"

pKeywordOpaque :: Parser (KeywordOpaque Span)
pKeywordOpaque = KeywordOpaque <$> keyword "opaque"

pKeywordOr :: Parser (KeywordOr Span)
pKeywordOr = KeywordOr <$> keyword "or"

pKeywordOrelse :: Parser (KeywordOrelse Span)
pKeywordOrelse = KeywordOrelse <$> keyword "orelse"

pKeywordPacked :: Parser (KeywordPacked Span)
pKeywordPacked = KeywordPacked <$> keyword "packed"

pKeywordPub :: Parser (KeywordPub Span)
pKeywordPub = KeywordPub <$> keyword "pub"

pKeywordResume :: Parser (KeywordResume Span)
pKeywordResume = KeywordResume <$> keyword "resume"

pKeywordReturn :: Parser (KeywordReturn Span)
pKeywordReturn = KeywordReturn <$> keyword "return"

pKeywordLinksection :: Parser (KeywordLinksection Span)
pKeywordLinksection = KeywordLinksection <$> keyword "linksection"

pKeywordStruct :: Parser (KeywordStruct Span)
pKeywordStruct = KeywordStruct <$> keyword "struct"

pKeywordSuspend :: Parser (KeywordSuspend Span)
pKeywordSuspend = KeywordSuspend <$> keyword "suspend"

pKeywordSwitch :: Parser (KeywordSwitch Span)
pKeywordSwitch = KeywordSwitch <$> keyword "switch"

pKeywordTest :: Parser (KeywordTest Span)
pKeywordTest = KeywordTest <$> keyword "test"

pKeywordThreadlocal :: Parser (KeywordThreadlocal Span)
pKeywordThreadlocal = KeywordThreadlocal <$> keyword "threadlocal"

pKeywordTrue :: Parser (KeywordTrue Span)
pKeywordTrue = KeywordTrue <$> keyword "true"

pKeywordTry :: Parser (KeywordTry Span)
pKeywordTry = KeywordTry <$> keyword "try"

pKeywordUndefined :: Parser (KeywordUndefined Span)
pKeywordUndefined = KeywordUndefined <$> keyword "undefined"

pKeywordUnion :: Parser (KeywordUnion Span)
pKeywordUnion = KeywordUnion <$> keyword "union"

pKeywordUnreachable :: Parser (KeywordUnreachable Span)
pKeywordUnreachable = KeywordUnreachable <$> keyword "unreachable"

pKeywordUsingnamespace :: Parser (KeywordUsingnamespace Span)
pKeywordUsingnamespace = KeywordUsingnamespace <$> keyword "usingnamespace"

pKeywordVar :: Parser (KeywordVar Span)
pKeywordVar = KeywordVar <$> keyword "var"

pKeywordVolatile :: Parser (KeywordVolatile Span)
pKeywordVolatile = KeywordVolatile <$> keyword "volatile"

pKeywordWhile :: Parser (KeywordWhile Span)
pKeywordWhile = KeywordWhile <$> keyword "while"

-- Weird:

pKeywordNoSuspend :: Parser (KeywordNoSuspend Span)
pKeywordNoSuspend = KeywordNoSuspend <$> keyword "nosuspend"
