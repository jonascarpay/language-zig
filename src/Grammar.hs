{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Grammar where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import GHC.Generics

data Sum f g a = This (f a) | That (g a)

-- Root <- skip ContainerMembers eof
data Root a = Root (ContainerMembers a)

-- ContainerMembers
--     <- TestDecl ContainerMembers
--      / TopLevelComptime ContainerMembers
--      / KEYWORD_pub? TopLevelDecl ContainerMembers
--      / ContainerField COMMA ContainerMembers
--      / ContainerField
--      /

data ContainerMembers a
  = CMTestDecl (TestDecl a) (ContainerMembers a)
  | CMTopLevelComptime (TopLevelComptime a) (ContainerMembers a)
  | CMTopLevelDecl (Maybe (KeywordPub a)) (TopLevelDecl a) (ContainerMembers a)
  | CMContainerField (ContainerField a) (Maybe (ContainerMembers a))
  | CMContainerEmpty

-- TestDecl <- KEYWORD_test STRINGLITERALSINGLE Block

data TestDecl a = TestDecl (KeywordTest a) (StringLiteralSingle a) (Block a)

-- TopLevelComptime <- KEYWORD_comptime BlockExpr

data TopLevelComptime a = TopLevelComptime (BlockExpr a)

-- TopLevelDecl
--     <- (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE? / KEYWORD_inline)? FnProto (SEMICOLON / Block)
--      / (KEYWORD_export / KEYWORD_extern STRINGLITERALSINGLE?)? KEYWORD_threadlocal? VarDecl
--      / KEYWORD_usingnamespace Expr SEMICOLON

data TopLevelDecl a
  = TLFnProto (Maybe (TLFnProtoQualifier a)) (FnProto a) (Maybe (Block a))
  | TLVarDecl (Maybe (TLVarDeclQualifier a)) (Maybe (KeywordThreadlocal a)) (VarDecl a)
  | TLUsingNamespace (KeywordUsingnamespace a) (Expr a)

data TLFnProtoQualifier a
  = FnProtoExport (KeywordExport a)
  | FnProtoExtern (KeywordExtern a) (Maybe (StringLiteralSingle a))
  | FnProtoInline (KeywordInline a)

data TLVarDeclQualifier a
  = VarExport (KeywordExport a)
  | VarExtern (KeywordExtern a) (Maybe (StringLiteralSingle a))

-- FnProto <- KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? LinkSection? EXCLAMATIONMARK? (KEYWORD_anytype / TypeExpr)

data FnProto a
  = FnProto
      (KeywordFn a)
      (Maybe (Identifier a))
      [ParamDecl a]
      (Maybe (ByteAlign a))
      (Maybe (LinkSection a))
      (Maybe a)
      (Sum KeywordAnytype TypeExpr a)

-- VarDecl <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? LinkSection? (EQUAL Expr)? SEMICOLON

data VarDecl a
  = VarDecl
      (VarDeclQualifier a)
      (Identifier a)
      (Maybe (TypeExpr a))
      (Maybe (ByteAlign a))
      (Maybe (LinkSection a))
      (Maybe (Expr a))

data VarDeclQualifier a
  = VarDeclConst (KeywordConst a)
  | VarDeclVar (KeywordVar a)

-- ContainerField <- KEYWORD_comptime? IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?

data ContainerField a
  = ContainerField
      (Maybe (KeywordComptime a))
      (Identifier a)
      (Maybe (TypeExpr a))
      (Maybe (Expr a))

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

data Statement a
  = StmtDecl (Maybe (KeywordComptime a)) (VarDecl a)
  | StmtComptime (KeywordComptime a) (BlockExprStatement a)
  | StmtNoSuspend (KeywordNoSuspend a) (BlockExprStatement a)
  | StmtSuspend (KeywordSuspend a) (Maybe (BlockExprStatement a))
  | StmtDefer (KeywordDefer a) (BlockExprStatement a)
  | StmtErrDefer (KeywordErrdefer a) (BlockExprStatement a)
  | StmtIf (IfStatement a)
  | StmtLabeled (LabeledStatement a)
  | StmtSwitch (SwitchExpr a)
  | StmtAssign (AssignExpr a)

-- IfStatement
--     <- IfPrefix BlockExpr ( KEYWORD_else Payload? Statement )?
--      / IfPrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )

data IfStatement a
  = IfBlock (IfPrefix a) (BlockExpr a) (Maybe ((KeywordElse a), Maybe (Payload a), (Statement a)))
  | IfAssign (IfPrefix a) (AssignExpr a) (Maybe ((KeywordElse a), Maybe (Payload a), (Statement a)))

-- LabeledStatement <- BlockLabel? (Block / LoopStatement)

data LabeledStatement a
  = LabeledBlock (Maybe (BlockLabel a)) (Block a)
  | LabeledLoop (Maybe (BlockLabel a)) (LoopStatement a)

-- LoopStatement <- KEYWORD_inline? (ForStatement / WhileStatement)

data LoopStatement a
  = LoopStmtFor (Maybe (KeywordInline a)) (ForStatement a)
  | LoopStmtWhile (Maybe (KeywordInline a)) (WhileStatement a)

-- ForStatement
--     <- ForPrefix BlockExpr ( KEYWORD_else Statement )?
--      / ForPrefix AssignExpr ( SEMICOLON / KEYWORD_else Statement )

data ForStatement a
  = ForBlock (ForPrefix a) (BlockExpr a) (Maybe ((KeywordElse a), (Statement a)))
  | ForAssign (ForPrefix a) (AssignExpr a) (Maybe ((KeywordElse a), (Statement a)))

-- WhileStatement
--     <- WhilePrefix BlockExpr ( KEYWORD_else Payload? Statement )?
--      / WhilePrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )

data WhileStatement a
  = WhileBlock (WhilePrefix a) (BlockExpr a) (Maybe ((KeywordElse a), (Statement a)))
  | WhileAssign (WhilePrefix a) (AssignExpr a) (Maybe ((KeywordElse a), (Statement a)))

-- BlockExprStatement
--     <- BlockExpr
--      / AssignExpr SEMICOLON

data BlockExprStatement a
  = BESBlock (BlockExpr a)
  | BESAssign (AssignExpr a)

-- BlockExpr <- BlockLabel? Block

data BlockExpr a = BlockExpr (Maybe (BlockLabel a)) (Block a)

-- # *** Expression Level ***
-- AssignExpr <- Expr (AssignOp Expr)?

data AssignExpr a = AssignExpr (Expr a) (Maybe (AssignOp a, Expr a))

-- Expr <- KEYWORD_try* BoolOrExpr

data Expr a = Expr [KeywordTry a] (BoolOrExpr a)

-- BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*

data BoolOrExpr a = BoolOrExpr (NonEmpty (BoolAndExpr a))

-- BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*

data BoolAndExpr a = BoolAndExpr (NonEmpty (CompareExpr a))

-- CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?

data CompareExpr a = CompareExpr (BitwiseExpr a) (Maybe (CompareOp a, BitwiseExpr a))

-- BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*

data BitwiseExpr a = BitwiseExpr (BitShiftExpr a) [(BitwiseOp a, BitShiftExpr a)]

-- BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*

data BitShiftExpr a = BitShiftExpr (AdditionExpr a) [(BitShiftOp a, AdditionExpr a)]

-- AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*

data AdditionExpr a = AdditionExpr (MultiplyExpr a) [(AdditionOp a, MultiplyExpr a)]

-- MultiplyExpr <- PrefixExpr (MultiplyOp PrefixExpr)*

data MultiplyExpr a = MultiplyExpr (PrefixExpr a) [(MultiplyOp a, PrefixExpr a)]

-- PrefixExpr <- PrefixOp* PrimaryExpr

data PrefixExpr a = PrefixExpr [PrefixOp a] (PrimaryExpr a)

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

data PrimaryExpr a
  = PrimAsm (AsmExpr a)
  | PrimIf (IfExpr a)
  | PrimBreak (KeywordBreak a) (Maybe (BreakLabel a)) (Maybe (Expr a))
  | PrimComptime (KeywordComptime a) (Expr a)
  | PrimNoSuspend (KeywordNoSuspend a) (Expr a)
  | PrimContinue (KeywordContinue a) (Maybe (BreakLabel a))
  | PrimResume (KeywordResume a) (Expr a)
  | PrimReturn (KeywordReturn a) (Maybe (Expr a))
  | PrimLoop (Maybe (BlockLabel a)) (LoopExpr a)
  | PrimBlock (Block a)
  | PrimCurlySuffixExpr (CurlySuffixExpr a)

-- IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?

data IfExpr a
  = IfExpr
      (IfPrefix a)
      (Expr a)
      ( Maybe
          ( KeywordElse a,
            Maybe (Payload a),
            Expr a
          )
      )

-- Block <- LBRACE Statement* RBRACE

data Block a = Block [Statement a]

-- LoopExpr <- KEYWORD_inline? (ForExpr / WhileExpr)

data LoopExpr a
  = LoopExprFor (Maybe (KeywordInline a)) (ForExpr a)
  | LoopExprWhile (Maybe (KeywordInline a)) (WhileExpr a)

-- ForExpr <- ForPrefix Expr (KEYWORD_else Expr)?

data ForExpr a = ForExpr (ForPrefix a) (Expr a) (Maybe (KeywordElse a, Expr a))

-- WhileExpr <- WhilePrefix Expr (KEYWORD_else Payload? Expr)?

data WhileExpr a = WhileExpr (WhilePrefix a) (Expr a) (Maybe (KeywordElse a, Maybe (Payload a), Expr a))

-- CurlySuffixExpr <- TypeExpr InitList?

data CurlySuffixExpr a = CurlySuffixExpr (TypeExpr a) (Maybe (InitList a))

-- InitList
--     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
--      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
--      / LBRACE RBRACE

data InitList a
  = InitFields (NonEmpty (FieldInit a))
  | InitExpr (NonEmpty (Expr a))
  | InitEmpty

-- TypeExpr <- PrefixTypeOp* ErrorUnionExpr

data TypeExpr a = TypeExpr [PrefixTypeOp a] (ErrorUnionExpr a)

-- ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?

data ErrorUnionExpr a = ErrorUnionExpr (SuffixExpr a) (Maybe (TypeExpr a))

-- SuffixExpr
--     <- KEYWORD_async PrimaryTypeExpr SuffixOp* FnCallArguments
--      / PrimaryTypeExpr (SuffixOp / FnCallArguments)*

data SuffixExpr a
  = SuffixAsync (KeywordAsync a) (PrimaryTypeExpr a) [SuffixOp a] (FnCallArguments a)
  | SuffixExpr (PrimaryTypeExpr a) [Sum SuffixOp FnCallArguments a]

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

data PrimaryTypeExpr a
  = PrimBuiltin (BuiltinIdentifier a) (FnCallArguments a)
  | PrimCharLiteral (CharLiteral a)
  | PrimContainer (ContainerDecl a)
  | PrimDotId (Identifier a)
  | PrimInitList (InitList a)
  | PrimErrorSet (ErrorSetDecl a)
  | PrimFloat (FloatLit a)
  | PrimFnProto (FnProto a)
  | PrimGrouped (GroupedExpr a)
  | PrimLabeledType (LabeledTypeExpr a)
  | PrimId (Identifier a)
  | PrimIfType (IfTypeExpr a)
  | PrimInt (IntLit a)
  | PrimType (KeywordComptime a) (TypeExpr a)
  | PrimErrId (KeywordError a) (Identifier a)
  | PrimFalse (KeywordFalse a)
  | PrimNull (KeywordNull a)
  | PrimAnyFrame (KeywordAnyframe a)
  | PrimTrue (KeywordTrue a)
  | PrimUndefined (KeywordUndefined a)
  | PrimUnreachable (KeywordUnreachable a)
  | PrimString (StringLit a)
  | PrimSwitch (SwitchExpr a)

-- ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto

data ContainerDecl a
  = ContainerExtern (KeywordExtern a) (ContainerDeclAuto a)
  | ContainerPacked (KeywordPacked a) (ContainerDeclAuto a)
  | ContainerDecl (ContainerDeclAuto a)

-- ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE

data ErrorSetDecl a = ErrorSetDecl (KeywordError a) (IdentifierList a)

-- GroupedExpr <- LPAREN Expr RPAREN

data GroupedExpr a = GroupedExpr (Expr a)

-- IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?

data IfTypeExpr a
  = IfTypeExpr
      (IfPrefix a)
      (TypeExpr a)
      ( Maybe
          ( KeywordElse a,
            Maybe (Payload a),
            TypeExpr a
          )
      )

-- LabeledTypeExpr
--     <- BlockLabel Block
--      / BlockLabel? LoopTypeExpr

data LabeledTypeExpr a
  = LabeledTypeBlock (BlockLabel a) (Block a)
  | LabeledTypeLoop (Maybe (BlockLabel a)) (LoopTypeExpr a)

-- LoopTypeExpr <- KEYWORD_inline? (ForTypeExpr / WhileTypeExpr)

data LoopTypeExpr a
  = LoopTypeFor (Maybe (KeywordInline a)) (ForTypeExpr a)
  | LoopTypeWhile (Maybe (KeywordInline a)) (WhileTypeExpr a)

-- ForTypeExpr <- ForPrefix TypeExpr (KEYWORD_else TypeExpr)?

data ForTypeExpr a = ForTypeExpr (ForPrefix a) (TypeExpr a) (Maybe (KeywordElse a, TypeExpr a))

-- WhileTypeExpr <- WhilePrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?

data WhileTypeExpr a
  = WhileTypeExpr
      (WhilePrefix a)
      (TypeExpr a)
      ( Maybe
          ( KeywordElse a,
            Maybe (Payload a),
            TypeExpr a
          )
      )

-- SwitchExpr <- KEYWORD_switch LPAREN Expr RPAREN LBRACE SwitchProngList RBRACE

data SwitchExpr a = SwitchExpr (KeywordSwitch a) (Expr a) (SwitchProngList a)

-- # *** Assembly ***
-- AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN STRINGLITERAL AsmOutput? RPAREN

data AsmExpr a
  = AsmExpr
      (KeywordAsm a)
      (Maybe (KeywordVolatile a))
      (StringLit a)
      (Maybe (AsmOutput a))

-- AsmOutput <- COLON AsmOutputList AsmInput?

data AsmOutput a = AsmOutput (AsmOutputList a) (Maybe (AsmInput a))

-- AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN

data AsmOutputItem a = AsmOutputItem (Identifier a) (StringLit a) (Sum TypeExpr Identifier a)

-- AsmInput <- COLON AsmInputList AsmClobbers?

data AsmInput a = AsmInput (AsmInputList a) (Maybe (AsmClobbers a))

-- AsmInputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN Expr RPAREN

data AsmInputItem a = AsmInputItem (Identifier a) (StringLit a) (Expr a)

-- AsmClobbers <- COLON StringList

data AsmClobbers a = AsmClobbers (StringList a)

-- # *** Helper grammar ***
-- BreakLabel <- COLON IDENTIFIER

data BreakLabel a = BreakLabel (Identifier a)

-- BlockLabel <- IDENTIFIER COLON

data BlockLabel a = BlockLabel (Identifier a)

-- FieldInit <- DOT IDENTIFIER EQUAL Expr

data FieldInit a = FieldInit (Identifier a) (Expr a)

-- WhileContinueExpr <- COLON LPAREN AssignExpr RPAREN

data WhileContinueExpr a = WhileContinueExpr (AssignExpr a)

-- LinkSection <- KEYWORD_linksection LPAREN Expr RPAREN

data LinkSection a = LinkSection (KeywordLinksection a) (Expr a)

-- ParamDecl <- (KEYWORD_noalias / KEYWORD_comptime)? (IDENTIFIER COLON)? ParamType

data ParamDecl a
  = ParamNoalias (KeywordNoalias a) (Maybe (Identifier a)) (ParamType a)
  | ParamComptime (KeywordComptime a) (Maybe (Identifier a)) (ParamType a)
  | ParamDecl (Maybe (Identifier a)) (ParamType a)

-- ParamType
--     <- KEYWORD_anytype
--      / DOT3
--      / TypeExpr

data ParamType a
  = ParamAnytype (KeywordAnytype a)
  | ParamDot3
  | ParamTypeExpr (TypeExpr a)

-- # Control flow prefixes
-- IfPrefix <- KEYWORD_if LPAREN Expr RPAREN PtrPayload?

data IfPrefix a = IfPrefix (KeywordIf a) (Expr a) (Maybe (PtrPayload a))

-- WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?

data WhilePrefix a = WhilePrefix (KeywordWhile a) (Expr a) (Maybe (PtrPayload a)) (Maybe (WhileContinueExpr a))

-- ForPrefix <- KEYWORD_for LPAREN Expr RPAREN PtrIndexPayload

data ForPrefix a = ForPrefix (KeywordFor a) (Expr a) (PtrIndexPayload a)

-- # Payloads
-- Payload <- PIPE IDENTIFIER PIPE

data Payload a = Payload (Identifier a)

data PtrAsterisk a
  = WithAsterisk a
  | WithoutAsterisk a

-- PtrPayload <- PIPE ASTERISK? IDENTIFIER PIPE

data PtrPayload a = PtrPayload (PtrAsterisk a) (Identifier a)

-- PtrIndexPayload <- PIPE ASTERISK? IDENTIFIER (COMMA IDENTIFIER)? PIPE

data PtrIndexPayload a = PtrIndexPayload (PtrAsterisk a) (Identifier a)

-- # Switch specific
-- SwitchProng <- SwitchCase EQUALRARROW PtrPayload? AssignExpr

data SwitchProng a = SwitchProng (SwitchCase a) (Maybe (PtrPayload a)) (AssignExpr a)

-- SwitchCase
--     <- SwitchItem (COMMA SwitchItem)* COMMA?
--      / KEYWORD_else

data SwitchCase a
  = CaseItem (NonEmpty (SwitchItem a))
  | CaseElse (KeywordElse a)

-- SwitchItem <- Expr (DOT3 Expr)?

data SwitchItem a = SwitchItem (Expr a) (Maybe (Expr a))

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

data AssignOp a
  = AssignAsteriskEqual a
  | AssignSlashEqual a
  | AssignPercentEqual a
  | AssignPlusEqual a
  | AssignMinusEqual a
  | AssignLarrow2Equal a
  | AssignRarrow2Equal a
  | AssignAmpersandEqual a
  | AssignCaretEqual a
  | AssignPipeEqual a
  | AssignAsteriskPercentEqual a
  | AssignPlusPercentEqual a
  | AssignMinusPercentEqual a
  | AssignEqual a

-- CompareOp
--     <- EQUALEQUAL
--      / EXCLAMATIONMARKEQUAL
--      / LARROW
--      / RARROW
--      / LARROWEQUAL
--      / RARROWEQUAL

data CompareOp a
  = CompareEqual a
  | CompareNotEqual a
  | CompareLarrow a
  | CompareRarrow a
  | CompareLarrowEqual a
  | CompareRarrowEqual a

-- BitwiseOp
--     <- AMPERSAND
--      / CARET
--      / PIPE
--      / KEYWORD_orelse
--      / KEYWORD_catch Payload?

data BitwiseOp a
  = BitwiseAmpersand a
  | BitwiseCaret a
  | BitwisePipe a
  | BitwiseOrElse (KeywordOrelse a)
  | BitwiseCatch (KeywordCatch a) (Maybe (Payload a))

-- BitShiftOp
--     <- LARROW2
--      / RARROW2

data BitShiftOp a
  = BitShiftLarrow2 a
  | BitShiftRarrow2 a

-- AdditionOp
--     <- PLUS
--      / MINUS
--      / PLUS2
--      / PLUSPERCENT
--      / MINUSPERCENT

data AdditionOp a
  = AdditionPlus a
  | AdditionMinus a
  | AdditionPlus2 a
  | AdditionPlusPercent a
  | AdditionMinusPercent a

-- MultiplyOp
--     <- PIPE2
--      / ASTERISK
--      / SLASH
--      / PERCENT
--      / ASTERISK2
--      / ASTERISKPERCENT

data MultiplyOp a
  = MultiplyPipe2 a
  | MultiplyAsterisk a
  | MultiplySlash a
  | MultiplyPercent a
  | MultiplyAsterisk2 a
  | MultiplyAsteriskPercent a

-- PrefixOp
--     <- EXCLAMATIONMARK
--      / MINUS
--      / TILDE
--      / MINUSPERCENT
--      / AMPERSAND
--      / KEYWORD_try
--      / KEYWORD_await

data PrefixOp a
  = PrefixNot a
  | PrefixMinus a
  | PrefixTilde a
  | PrefixMinusPercent a
  | PrefixAmpersand a
  | PrefixTry (KeywordTry a)
  | PrefixAwait (KeywordAwait a)

-- PrefixTypeOp
--     <- QUESTIONMARK
--      / KEYWORD_anyframe MINUSRARROW
--      / ArrayTypeStart (ByteAlign / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*
--      / PtrTypeStart (KEYWORD_align LPAREN Expr (COLON INTEGER COLON INTEGER)? RPAREN / KEYWORD_const / KEYWORD_volatile / KEYWORD_allowzero)*

data PrefixTypeOp a
  = PrefixTypeQuestion a
  | PrefixTypeAnyFrame (KeywordAnyframe a)
  | PrefixTypeArrayTypeStart [PrefixTypeArrayStartQualifier a]
  | PrefixTypePtrTypeStart [PrefixTypePtrStartQualifier a]

data PrefixTypeArrayStartQualifier a
  = PrefixTASQAlign (ByteAlign a)
  | PrefixTASQConst (KeywordConst a)
  | PrefixTASQVolatile (KeywordVolatile a)
  | PrefixTASQAllowZero (KeywordAllowzero a)

data PrefixTypePtrStartQualifier a
  = PrefixTPSQAlign (KeywordAlign a) (Expr a) (Maybe (IntLit a, IntLit a))
  | PrefixTPSQConst (KeywordConst a)
  | PrefixTPSQVolatile (KeywordVolatile a)
  | PrefixTPSQAllowZero (KeywordAllowzero a)

-- SuffixOp
--     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
--      / DOT IDENTIFIER
--      / DOTASTERISK
--      / DOTQUESTIONMARK

data SuffixOp a
  = SuffixOpExpr (Expr a) (Maybe (Maybe (Expr a)))
  | SuffixOpId (Identifier a)
  | SuffixOpAsterisk
  | SuffixOpQuestion

-- FnCallArguments <- LPAREN ExprList RPAREN

data FnCallArguments a = FnCallArguments (ExprList a)

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

data ContainerDeclAuto a = ContainerDeclAuto (ContainerDeclType a) (ContainerMembers a)

-- ContainerDeclType
--     <- (KEYWORD_struct / KEYWORD_enum / KEYWORD_opaque) (LPAREN Expr RPAREN)?
--      / KEYWORD_union (LPAREN (KEYWORD_enum (LPAREN Expr RPAREN)? / Expr) RPAREN)?

data ContainerDeclType a
  = ContStruct (KeywordStruct a) (Maybe (Expr a))
  | ContEnum (KeywordEnum a) (Maybe (Expr a))
  | ContOpaque (KeywordOpaque a) (Maybe (Expr a))
  | ContEmptyUnion (KeywordUnion a)
  | ContEnumUnion (KeywordUnion a) (KeywordEnum a) (Maybe (Expr a))
  | ContUnion (KeywordUnion a) (Expr a)

-- # Alignment
-- ByteAlign <- KEYWORD_align LPAREN Expr RPAREN

data ByteAlign a = ByteAlign (KeywordAlign a) (Expr a)

-- # Lists
-- IdentifierList <- (IDENTIFIER COMMA)* IDENTIFIER?

data IdentifierList a = IdentifierList [Identifier a]

-- SwitchProngList <- (SwitchProng COMMA)* SwitchProng?

data SwitchProngList a = SwitchProngList [SwitchProng a]

-- AsmOutputList <- (AsmOutputItem COMMA)* AsmOutputItem?

data AsmOutputList a = AsmOutputList [AsmOutputItem a]

-- AsmInputList <- (AsmInputItem COMMA)* AsmInputItem?

data AsmInputList a = AsmInputList [AsmInputItem a]

-- StringList <- (STRINGLITERAL COMMA)* STRINGLITERAL?

data StringList a = StringList [StringLit a]

-- ParamDeclList <- (ParamDecl COMMA)* ParamDecl?
--
-- ExprList <- (Expr COMMA)* Expr?

data ExprList a = ExprList [Expr a]

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

data LineString a = LineString a ByteString

-- skip <- ([ \n] / line_comment)*
--
-- CHAR_LITERAL <- "'" char_char "'" skip

data CharLiteral a = CharLiteral a Char

-- FLOAT
--     <- "0x" hex+   "." hex+   ([pP] [-+]? hex+)?   skip
--      /      [0-9]+ "." [0-9]+ ([eE] [-+]? [0-9]+)? skip
--      / "0x" hex+   "."? [pP] [-+]? hex+   skip
--      /      [0-9]+ "."? [eE] [-+]? [0-9]+ skip

data FloatLit a = FloatLit a Double -- FIXME Scary

--
-- INTEGER
--     <- "0b" [01]+  skip
--      / "0o" [0-7]+ skip
--      / "0x" hex+   skip
--      /      [0-9]+ skip

data IntLit a = IntLit a Integer

-- STRINGLITERALSINGLE <- "\"" string_char* "\"" skip

data StringLiteralSingle a = StringLiteralSingle a ByteString

-- STRINGLITERAL
--     <- STRINGLITERALSINGLE
--      / line_string                 skip

data StringLit a
  = StringLitSingle (StringLiteralSingle a)
  | StringLitLine (LineString a)

-- IDENTIFIER
--     <- !keyword [A-Za-z_] [A-Za-z0-9_]* skip
--      / "@\"" string_char* "\""                            skip

data Identifier a
  = Identifier a ByteString
  | AtIdentifier a ByteString

-- BUILTINIDENTIFIER <- "@"[A-Za-z_][A-Za-z0-9_]* skip

data BuiltinIdentifier a = BuiltinIdentifier a ByteString

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

newtype KeywordAlign a = KeywordAlign {unKeywordAlign :: a}

newtype KeywordAnd a = KeywordAnd {unKeywordAnd :: a}

newtype KeywordAnyframe a = KeywordAnyframe {unKeywordAnyframe :: a}

newtype KeywordAnytype a = KeywordAnytype {unKeywordAnytype :: a}

newtype KeywordAllowzero a = KeywordAllowzero {unKeywordAllowzero :: a}

newtype KeywordAsm a = KeywordAsm {unKeywordAsm :: a}

newtype KeywordAsync a = KeywordAsync {unKeywordAsync :: a}

newtype KeywordAwait a = KeywordAwait {unKeywordAwait :: a}

newtype KeywordBreak a = KeywordBreak {unKeywordBreak :: a}

newtype KeywordCatch a = KeywordCatch {unKeywordCatch :: a}

newtype KeywordComptime a = KeywordComptime {unKeywordComptime :: a}

newtype KeywordConst a = KeywordConst {unKeywordConst :: a}

newtype KeywordContinue a = KeywordContinue {unKeywordContinue :: a}

newtype KeywordDefer a = KeywordDefer {unKeywordDefer :: a}

newtype KeywordElse a = KeywordElse {unKeywordElse :: a}

newtype KeywordEnum a = KeywordEnum {unKeywordEnum :: a}

newtype KeywordErrdefer a = KeywordErrdefer {unKeywordErrdefer :: a}

newtype KeywordError a = KeywordError {unKeywordError :: a}

newtype KeywordExport a = KeywordExport {unKeywordExport :: a}

newtype KeywordExtern a = KeywordExtern {unKeywordExtern :: a}

newtype KeywordFalse a = KeywordFalse {unKeywordFalse :: a}

newtype KeywordFor a = KeywordFor {unKeywordFor :: a}

newtype KeywordFn a = KeywordFn {unKeywordFn :: a}

newtype KeywordIf a = KeywordIf {unKeywordIf :: a}

newtype KeywordInline a = KeywordInline {unKeywordInline :: a}

newtype KeywordNoalias a = KeywordNoalias {unKeywordNoalias :: a}

newtype KeywordNull a = KeywordNull {unKeywordNull :: a}

newtype KeywordOpaque a = KeywordOpaque {unKeywordOpaque :: a}

newtype KeywordOr a = KeywordOr {unKeywordOr :: a}

newtype KeywordOrelse a = KeywordOrelse {unKeywordOrelse :: a}

newtype KeywordPacked a = KeywordPacked {unKeywordPacked :: a}

newtype KeywordPub a = KeywordPub {unKeywordPub :: a}

newtype KeywordResume a = KeywordResume {unKeywordResume :: a}

newtype KeywordReturn a = KeywordReturn {unKeywordReturn :: a}

newtype KeywordLinksection a = KeywordLinksection {unKeywordLinksection :: a}

newtype KeywordStruct a = KeywordStruct {unKeywordStruct :: a}

newtype KeywordSuspend a = KeywordSuspend {unKeywordSuspend :: a}

newtype KeywordSwitch a = KeywordSwitch {unKeywordSwitch :: a}

newtype KeywordTest a = KeywordTest {unKeywordTest :: a}

newtype KeywordThreadlocal a = KeywordThreadlocal {unKeywordThreadlocal :: a}

newtype KeywordTrue a = KeywordTrue {unKeywordTrue :: a}

newtype KeywordTry a = KeywordTry {unKeywordTry :: a}

newtype KeywordUndefined a = KeywordUndefined {unKeywordUndefined :: a}

newtype KeywordUnion a = KeywordUnion {unKeywordUnion :: a}

newtype KeywordUnreachable a = KeywordUnreachable {unKeywordUnreachable :: a}

newtype KeywordUsingnamespace a = KeywordUsingnamespace {unKeywordUsingnamespace :: a}

newtype KeywordVar a = KeywordVar {unKeywordVar :: a}

newtype KeywordVolatile a = KeywordVolatile {unKeywordVolatile :: a}

newtype KeywordWhile a = KeywordWhile {unKeywordWhile :: a}

-- Weird:

newtype KeywordNoSuspend a = KeywordNoSuspend {unKeywordNoSuspend :: a}
