{-# LANGUAGE DeriveGeneric #-}

module Eval where

import AST
import Data.Map (Map)
import Data.Word
import GHC.Generics (Generic)

data Signedness = Signed | Unsigned
  deriving (Eq, Show, Generic)

data Type = Integer {signed :: Signedness, bits :: Word16}

data TypeError

type Context = Map Identifier Type

type Env = Map Identifier Value

atType :: Expression -> Env -> Either TypeError Type
atType = undefined

evalExpr :: Expression -> Env -> Either EvalError Value
evalExpr (Int i) _ = pure $ ComptimeInt i
evalExpr (Mul a b) env = do
  x <- evalExpr a env
  y <- evalExpr b env
  multiply x y

data EvalError
  = TODO
  | NoMainFunction

eval :: Zig -> Either EvalError Word8
eval = undefined
