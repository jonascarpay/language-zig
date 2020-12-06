{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}

module AST where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as M
import GHC.Generics

-- Some day: pAST :: Parser AST

newtype Zig = Zig StructDef
  deriving (Eq, Show, Generic)

data Identifier = Identifier ByteString
  deriving (Eq, Ord, Show, Generic)

data ParamDecl
  deriving (Eq, Show, Generic)

data ContainerMember
  = TlFunction Visibility Function
  --- | TlVar Visibility Declaration
  --- | TlUsingNamespace Visibility
  --- | Test TestDeclaration
  --- | Comptime { block }
  --- | ContainerField
  deriving (Eq, Show, Generic)

data Visibility = Private | Public
  deriving (Eq, Show, Enum, Generic)

data CompileTime = Runtime | CompileTime
  deriving (Eq, Show, Enum, Generic)

newtype Alignment = Alignment Expression
  deriving (Eq, Show, Generic)
newtype Linking = Linking Expression
  deriving (Eq, Show, Generic)

data ReturnType = AnyType | TypeExpression Expression
  deriving (Eq, Show, Generic)

data Function = Function
  { fnId :: Identifier
  , fnParams :: [ParamDecl]
  , fnAlign :: Maybe Alignment
  , fnLink :: Maybe Linking
  , fnRetType :: ReturnType
  , fnBody :: [Statement]
  }
  deriving (Eq, Show, Generic)

data Expression
  = Int Integer
  | Mul Expression Expression
  | Or Expression Expression
  | And Expression Expression
  | Ret (Maybe Expression)
  | Try Expression
  | CurlySuffix Expression InitList
  | Questionmark Expression
  | ErrorUnion Expression Expression
  | IdentifierExpr Identifier
  | StructDefExpr StructDef
  deriving (Eq, Show, Generic)

-- TODO
newtype Field = Field ()
  deriving (Eq, Show, Generic)

data StructDef = StructDef
  { sdField :: Map Identifier Field
  , sdFunction :: Map Identifier (Visibility, Function)
  }
  deriving (Eq, Show, Generic)

data ContainerQualifier = Extern | Packed
  deriving (Eq, Show, Generic)

data ContainerType
  = Struct (Maybe Expression)
  deriving (Eq, Show, Generic)

data Constness = Var | Const
  deriving (Eq, Show, Generic)

data InitList
  deriving (Eq, Show, Generic)

data Declaration = Declaration
  { varQualifier :: CompileTime -- TODO have this here?
  , varConst :: Constness
  , varId :: Maybe Identifier
  , varType :: Maybe Expression
  , varAlign :: Maybe Alignment
  , varLink :: Maybe Linking
  , varValue :: Maybe Expression
  }
  deriving (Eq, Show, Generic)

data Statement
  = DeclarationStatement Declaration
  | AssignmentStatement Expression AssignOp Expression -- var = value
  | ExpressionStatement Expression -- value
  deriving (Eq, Show, Generic)

data AssignOp
  deriving (Eq, Show, Generic)
