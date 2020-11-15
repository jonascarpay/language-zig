{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.ByteString (ByteString)
import Data.Void (Void)
import Grammar
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lex

type Parser = Parsec Void ByteString

data Span = Span Int Int
  deriving (Show, Eq)

sc :: Parser ()
sc =
  Lex.space
    space1
    (Lex.skipLineComment "//")
    empty

pKeyword :: ByteString -> Parser Span
pKeyword str = do
  a <- stateOffset <$> getParserState
  _ <- chunk str
  b <- stateOffset <$> getParserState
  sc
  pure (Span a b)

pKeywordAlign :: Parser (KeywordAlign Span)
pKeywordAlign = KeywordAlign <$> pKeyword "align"

pKeywordAnd :: Parser (KeywordAnd Span)
pKeywordAnd = KeywordAnd <$> pKeyword "and"

pKeywordAnyframe :: Parser (KeywordAnyframe Span)
pKeywordAnyframe = KeywordAnyframe <$> pKeyword "anyframe"

pKeywordAnytype :: Parser (KeywordAnytype Span)
pKeywordAnytype = KeywordAnytype <$> pKeyword "anytype"

pKeywordAllowzero :: Parser (KeywordAllowzero Span)
pKeywordAllowzero = KeywordAllowzero <$> pKeyword "allowzero"

pKeywordAsm :: Parser (KeywordAsm Span)
pKeywordAsm = KeywordAsm <$> pKeyword "asm"

pKeywordAsync :: Parser (KeywordAsync Span)
pKeywordAsync = KeywordAsync <$> pKeyword "async"

pKeywordAwait :: Parser (KeywordAwait Span)
pKeywordAwait = KeywordAwait <$> pKeyword "await"

pKeywordBreak :: Parser (KeywordBreak Span)
pKeywordBreak = KeywordBreak <$> pKeyword "break"

pKeywordCatch :: Parser (KeywordCatch Span)
pKeywordCatch = KeywordCatch <$> pKeyword "catch"

pKeywordComptime :: Parser (KeywordComptime Span)
pKeywordComptime = KeywordComptime <$> pKeyword "comptime"

pKeywordConst :: Parser (KeywordConst Span)
pKeywordConst = KeywordConst <$> pKeyword "const"

pKeywordContinue :: Parser (KeywordContinue Span)
pKeywordContinue = KeywordContinue <$> pKeyword "continue"

pKeywordDefer :: Parser (KeywordDefer Span)
pKeywordDefer = KeywordDefer <$> pKeyword "defer"

pKeywordElse :: Parser (KeywordElse Span)
pKeywordElse = KeywordElse <$> pKeyword "else"

pKeywordEnum :: Parser (KeywordEnum Span)
pKeywordEnum = KeywordEnum <$> pKeyword "enum"

pKeywordErrdefer :: Parser (KeywordErrdefer Span)
pKeywordErrdefer = KeywordErrdefer <$> pKeyword "errdefer"

pKeywordError :: Parser (KeywordError Span)
pKeywordError = KeywordError <$> pKeyword "error"

pKeywordExport :: Parser (KeywordExport Span)
pKeywordExport = KeywordExport <$> pKeyword "export"

pKeywordExtern :: Parser (KeywordExtern Span)
pKeywordExtern = KeywordExtern <$> pKeyword "extern"

pKeywordFalse :: Parser (KeywordFalse Span)
pKeywordFalse = KeywordFalse <$> pKeyword "false"

pKeywordFor :: Parser (KeywordFor Span)
pKeywordFor = KeywordFor <$> pKeyword "for"

pKeywordFn :: Parser (KeywordFn Span)
pKeywordFn = KeywordFn <$> pKeyword "fn"

pKeywordIf :: Parser (KeywordIf Span)
pKeywordIf = KeywordIf <$> pKeyword "if"

pKeywordInline :: Parser (KeywordInline Span)
pKeywordInline = KeywordInline <$> pKeyword "inline"

pKeywordNoalias :: Parser (KeywordNoalias Span)
pKeywordNoalias = KeywordNoalias <$> pKeyword "noalias"

pKeywordNull :: Parser (KeywordNull Span)
pKeywordNull = KeywordNull <$> pKeyword "null"

pKeywordOpaque :: Parser (KeywordOpaque Span)
pKeywordOpaque = KeywordOpaque <$> pKeyword "opaque"

pKeywordOr :: Parser (KeywordOr Span)
pKeywordOr = KeywordOr <$> pKeyword "or"

pKeywordOrelse :: Parser (KeywordOrelse Span)
pKeywordOrelse = KeywordOrelse <$> pKeyword "orelse"

pKeywordPacked :: Parser (KeywordPacked Span)
pKeywordPacked = KeywordPacked <$> pKeyword "packed"

pKeywordPub :: Parser (KeywordPub Span)
pKeywordPub = KeywordPub <$> pKeyword "pub"

pKeywordResume :: Parser (KeywordResume Span)
pKeywordResume = KeywordResume <$> pKeyword "resume"

pKeywordReturn :: Parser (KeywordReturn Span)
pKeywordReturn = KeywordReturn <$> pKeyword "return"

pKeywordLinksection :: Parser (KeywordLinksection Span)
pKeywordLinksection = KeywordLinksection <$> pKeyword "linksection"

pKeywordStruct :: Parser (KeywordStruct Span)
pKeywordStruct = KeywordStruct <$> pKeyword "struct"

pKeywordSuspend :: Parser (KeywordSuspend Span)
pKeywordSuspend = KeywordSuspend <$> pKeyword "suspend"

pKeywordSwitch :: Parser (KeywordSwitch Span)
pKeywordSwitch = KeywordSwitch <$> pKeyword "switch"

pKeywordTest :: Parser (KeywordTest Span)
pKeywordTest = KeywordTest <$> pKeyword "test"

pKeywordThreadlocal :: Parser (KeywordThreadlocal Span)
pKeywordThreadlocal = KeywordThreadlocal <$> pKeyword "threadlocal"

pKeywordTrue :: Parser (KeywordTrue Span)
pKeywordTrue = KeywordTrue <$> pKeyword "true"

pKeywordTry :: Parser (KeywordTry Span)
pKeywordTry = KeywordTry <$> pKeyword "try"

pKeywordUndefined :: Parser (KeywordUndefined Span)
pKeywordUndefined = KeywordUndefined <$> pKeyword "undefined"

pKeywordUnion :: Parser (KeywordUnion Span)
pKeywordUnion = KeywordUnion <$> pKeyword "union"

pKeywordUnreachable :: Parser (KeywordUnreachable Span)
pKeywordUnreachable = KeywordUnreachable <$> pKeyword "unreachable"

pKeywordUsingnamespace :: Parser (KeywordUsingnamespace Span)
pKeywordUsingnamespace = KeywordUsingnamespace <$> pKeyword "usingnamespace"

pKeywordVar :: Parser (KeywordVar Span)
pKeywordVar = KeywordVar <$> pKeyword "var"

pKeywordVolatile :: Parser (KeywordVolatile Span)
pKeywordVolatile = KeywordVolatile <$> pKeyword "volatile"

pKeywordWhile :: Parser (KeywordWhile Span)
pKeywordWhile = KeywordWhile <$> pKeyword "while"

-- Weird:

pKeywordNoSuspend :: Parser (KeywordNoSuspend Span)
pKeywordNoSuspend = KeywordNoSuspend <$> pKeyword "nosuspend"

-- Root <- skip ContainerMembers eof
--
-- # *** Top level ***
-- ContainerMembers
--     <- TestDecl ContainerMembers
--      / TopLevelComptime ContainerMembers
--      / KEYWORD_pub? TopLevelDecl ContainerMembers
--      / ContainerField COMMA ContainerMembers
--      / ContainerField
--      /
--
-- TestDecl <- KEYWORD_test STRINGLITERALSINGLE Block
--
-- TopLevelComptime <- KEYWORD_comptime BlockExpr
--
-- TopLevelDecl
--     <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / KEYWORD_inline)? FnProto (SEMICOLON / Block)
--      / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? VarDecl
--      / KEYWORD_usingnamespace Expr SEMICOLON
--
-- FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? LinkSection? EXCLAMATIONMARK? (KEYWORD_anytype / TypeExpr)
--
-- VarDecl <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? LinkSection? (EQUAL Expr)? SEMICOLON
--
-- ContainerField <- KEYWORD_comptime? IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?
--
-- # *** Block Level ***
-- Statement
--     <- KEYWORD_comptime? VarDecl
--      / KEYWORD_comptime BlockExprStatement
--      / KEYWORD_nosuspend BlockExprStatement
--      / KEYWORD_suspend (SEMICOLON / BlockExprStatement)
--      / KEYWORD_defer BlockExprStatement
--      / KEYWORD_errdefer BlockExprStatement
--      / IfStatement
--      / LabeledStatement
--      / SwitchExpr
--      / AssignExpr SEMICOLON
--
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
--
-- # *** Expression Level ***
-- AssignExpr <- Expr (AssignOp Expr)?
--
-- Expr <- KEYWORD_try* BoolOrExpr
--
-- BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*
--
-- BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*
--
-- CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?
--
-- BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*
--
-- BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*
--
-- AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*
--
-- MultiplyExpr <- PrefixExpr (MultiplyOp PrefixExpr)*
--
-- PrefixExpr <- PrefixOp* PrimaryExpr
--
-- PrimaryExpr
--     <- AsmExpr
--      / IfExpr
--      / KEYWORD_break BreakLabel? Expr?
--      / KEYWORD_comptime Expr
--      / KEYWORD_nosuspend Expr
--      / KEYWORD_continue BreakLabel?
--      / KEYWORD_resume Expr
--      / KEYWORD_return Expr?
--      / BlockLabel? LoopExpr
--      / Block
--      / CurlySuffixExpr
--
-- IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?
--
-- Block <- LBRACE Statement* RBRACE
--
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
--
-- ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
--
-- SuffixExpr
--     <- KEYWORD_async PrimaryTypeExpr SuffixOp* FnCallArguments
--      / PrimaryTypeExpr (SuffixOp / FnCallArguments)*
--
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
--
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
--
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
--
-- CompareOp
--     <- EQUALEQUAL
--      / EXCLAMATIONMARKEQUAL
--      / LARROW
--      / RARROW
--      / LARROWEQUAL
--      / RARROWEQUAL
--
-- BitwiseOp
--     <- AMPERSAND
--      / CARET
--      / PIPE
--      / KEYWORD_orelse
--      / KEYWORD_catch Payload?
--
-- BitShiftOp
--     <- LARROW2
--      / RARROW2
--
-- AdditionOp
--     <- PLUS
--      / MINUS
--      / PLUS2
--      / PLUSPERCENT
--      / MINUSPERCENT
--
-- MultiplyOp
--     <- PIPE2
--      / ASTERISK
--      / SLASH
--      / PERCENT
--      / ASTERISK2
--      / ASTERISKPERCENT
--
-- PrefixOp
--     <- EXCLAMATIONMARK
--      / MINUS
--      / TILDE
--      / MINUSPERCENT
--      / AMPERSAND
--      / KEYWORD_try
--      / KEYWORD_await
--
-- PrefixTypeOp
--     <- QUESTIONMARK
--      / KEYWORD_anyframe MINUSRARROW
--      / ArrayTypeStart (ByteAlign / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
--      / PtrTypeStart (KEYWORD_align LPAREN Expr (COLON INTEGER COLON INTEGER)? RPAREN / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
--
-- SuffixOp
--     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
--      / DOT IDENTIFIER
--      / DOTASTERISK
--      / DOTQUESTIONMARK
--
-- FnCallArguments <- LPAREN ExprList RPAREN
--
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
--
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
--
-- end_of_word <- ![a-zA-Z0-9_] skip
-- KEYWORD_align       <- 'align'       end_of_word
-- KEYWORD_allowzero   <- 'allowzero'   end_of_word
-- KEYWORD_and         <- 'and'         end_of_word
-- KEYWORD_anyframe    <- 'anyframe'    end_of_word
-- KEYWORD_anytype     <- 'anytype'     end_of_word
-- KEYWORD_asm         <- 'asm'         end_of_word
-- KEYWORD_async       <- 'async'       end_of_word
-- KEYWORD_await       <- 'await'       end_of_word
-- KEYWORD_break       <- 'break'       end_of_word
-- KEYWORD_catch       <- 'catch'       end_of_word
-- KEYWORD_comptime    <- 'comptime'    end_of_word
-- KEYWORD_const       <- 'const'       end_of_word
-- KEYWORD_continue    <- 'continue'    end_of_word
-- KEYWORD_defer       <- 'defer'       end_of_word
-- KEYWORD_else        <- 'else'        end_of_word
-- KEYWORD_enum        <- 'enum'        end_of_word
-- KEYWORD_errdefer    <- 'errdefer'    end_of_word
-- KEYWORD_error       <- 'error'       end_of_word
-- KEYWORD_export      <- 'export'      end_of_word
-- KEYWORD_extern      <- 'extern'      end_of_word
-- KEYWORD_false       <- 'false'       end_of_word
-- KEYWORD_fn          <- 'fn'          end_of_word
-- KEYWORD_for         <- 'for'         end_of_word
-- KEYWORD_if          <- 'if'          end_of_word
-- KEYWORD_inline      <- 'inline'      end_of_word
-- KEYWORD_noalias     <- 'noalias'     end_of_word
-- KEYWORD_nosuspend   <- 'nosuspend'   end_of_word
-- KEYWORD_null        <- 'null'        end_of_word
-- KEYWORD_opaque      <- 'opaque'      end_of_word
-- KEYWORD_or          <- 'or'          end_of_word
-- KEYWORD_orelse      <- 'orelse'      end_of_word
-- KEYWORD_packed      <- 'packed'      end_of_word
-- KEYWORD_pub         <- 'pub'         end_of_word
-- KEYWORD_resume      <- 'resume'      end_of_word
-- KEYWORD_return      <- 'return'      end_of_word
-- KEYWORD_linksection <- 'linksection' end_of_word
-- KEYWORD_struct      <- 'struct'      end_of_word
-- KEYWORD_suspend     <- 'suspend'     end_of_word
-- KEYWORD_switch      <- 'switch'      end_of_word
-- KEYWORD_test        <- 'test'        end_of_word
-- KEYWORD_threadlocal <- 'threadlocal' end_of_word
-- KEYWORD_true        <- 'true'        end_of_word
-- KEYWORD_try         <- 'try'         end_of_word
-- KEYWORD_undefined   <- 'undefined'   end_of_word
-- KEYWORD_union       <- 'union'       end_of_word
-- KEYWORD_unreachable <- 'unreachable' end_of_word
-- KEYWORD_usingnamespace <- 'usingnamespace' end_of_word
-- KEYWORD_var         <- 'var'         end_of_word
-- KEYWORD_volatile    <- 'volatile'    end_of_word
-- KEYWORD_while       <- 'while'       end_of_word
--
-- keyword <- KEYWORD_align / KEYWORD_and / KEYWORD_anyframe / KEYWORD_anytype
--          / KEYWORD_allowzero / KEYWORD_asm / KEYWORD_async / KEYWORD_await / KEYWORD_break
--          / KEYWORD_catch / KEYWORD_comptime / KEYWORD_const / KEYWORD_continue
--          / KEYWORD_defer / KEYWORD_else / KEYWORD_enum / KEYWORD_errdefer
--          / KEYWORD_error / KEYWORD_export / KEYWORD_extern / KEYWORD_false
--          / KEYWORD_fn / KEYWORD_for / KEYWORD_if / KEYWORD_inline
--          / KEYWORD_noalias / KEYWORD_null / KEYWORD_opaque / KEYWORD_or
--          / KEYWORD_orelse / KEYWORD_packed / KEYWORD_pub
--          / KEYWORD_resume / KEYWORD_return / KEYWORD_linksection
--          / KEYWORD_struct / KEYWORD_suspend
--          / KEYWORD_switch / KEYWORD_test / KEYWORD_threadlocal / KEYWORD_true / KEYWORD_try
--          / KEYWORD_undefined / KEYWORD_union / KEYWORD_unreachable
--          / KEYWORD_usingnamespace / KEYWORD_var / KEYWORD_volatile / KEYWORD_while
