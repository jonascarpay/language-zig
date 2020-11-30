{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}

module AST where

import Data.ByteString (ByteString)
import Data.Typeable
import GHC.Generics

-- Some day: pAST :: Parser AST

data Zig = Zig [TopLevel]
  deriving (Eq, Show, Typeable, Generic)

data Identifier = Identifier ByteString
  deriving (Eq, Show, Typeable, Generic)

data ParamDecl
  deriving (Eq, Show, Typeable, Generic)

data TopLevel
  = TlFunction Visibility Function
  --- | TlVar Visibility Declaration
  --- | TlUsingNamespace Visibility
  deriving (Eq, Show, Typeable, Generic)

-- Test TestDeclaration
-- Comptime
-- ContainerFiel

data Visibility = Private | Public
  deriving (Eq, Show, Enum, Typeable, Generic)

data CompileTime = Runtime | CompileTime
  deriving (Eq, Show, Enum, Typeable, Generic)

newtype Alignment = Alignment Expression
  deriving (Eq, Show, Typeable, Generic)
newtype Linking = Linking Expression
  deriving (Eq, Show, Typeable, Generic)

data ReturnType = AnyType | TypeExpression TypeExpression
  deriving (Eq, Show, Typeable, Generic)

data TypeExpression
  = Nullable TypeExpression
  | TypeVariable Identifier
  deriving (Eq, Show, Typeable, Generic)

data Function
  = Function
      Identifier
      [ParamDecl]
      (Maybe Alignment)
      (Maybe Linking)
      ReturnType
      [Statement]
  deriving (Eq, Show, Typeable, Generic)

data Expression
  = Int Integer
  | Mul Expression Expression
  | Or Expression Expression
  | And Expression Expression
  | Ret (Maybe Expression)
  | Try Expression
  deriving (Eq, Show, Typeable, Generic)

data Constness = Var | Const
  deriving (Eq, Show, Typeable, Generic)

data Declaration
  = Declaration
      CompileTime
      Constness
      (Maybe Identifier)
      (Maybe TypeExpression)
      (Maybe Alignment)
      (Maybe Linking)
      (Maybe Expression)
  deriving (Eq, Show, Typeable, Generic)

data Statement
  = DeclarationStatement Declaration
  deriving (Eq, Show, Typeable, Generic)
