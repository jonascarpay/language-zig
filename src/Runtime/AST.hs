{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Runtime.AST where

import Data.Map (Map)
import Data.Void
import Data.Word
import Runtime.Value

data Cofree f a = (:<) {extract :: a, unwrap :: f (Cofree f a)}
  deriving (Functor, Foldable, Traversable)

type Name = String

type a :< f = Cofree f a

-- TODO unify var and fun in symbol? They're both addresses, but
--   decl is always local/relative
--   fun is always global
--   var is either
data FunctionDecl decl var fun finfo t = FunctionDecl
  { funArgs :: [decl],
    funRet :: Type,
    funInfo :: finfo,
    funBody :: Scope decl var fun t
  }

newtype Scope d v f t = Scope {unScope :: [Statement d v f t]}

data ExprF var fun e
  = MulF e e
  | LitF (Value Void)
  | VarF var
  | CallF fun [e]

data Statement d v f t
  = Return (Expr v f t)
  | Declare d -- TODO put type in `d`?
  | Assign v (Expr v f t)

newtype Program decl var fun finfo t = Program {progFunctions :: Map Name (FunctionDecl decl var fun finfo t)}

type Expr var fun t = t :< ExprF var fun

type UExpr = Expr Name Name ()

type TExpr = Expr Name Name Type

type UStatement = Statement (Name, Type) Name Name ()

type TStatement = Statement (Name, Type) Name Name Type

type UFunctionDecl = FunctionDecl (Name, Type) Name Name () ()

type TFunctionDecl = FunctionDecl (Name, Type) Name Name () Type

type UProgram = Program (Name, Type) Name Name () ()

type TProgram = Program (Name, Type) Name Name () Type

instance Num (Expr var fun ()) where
  fromInteger = Lit . fromInteger
  a * b = Mul a b

pattern Mul :: Expr var fun () -> Expr var fun () -> Expr var fun ()
pattern Mul l r = () :< MulF l r

pattern Lit :: Word8 -> Expr var fun ()
pattern Lit n = () :< LitF (VU8 n)

type Traversal s t a b = forall m. Applicative m => (a -> m b) -> (s -> m t)

exprVars :: Traversal (Expr v f t) (Expr v' f t) v v'
exprVars f = go
  where
    go (t :< VarF v) = (\v' -> t :< VarF v') <$> f v
    go (t :< LitF v) = pure $ t :< LitF v
    go (t :< CallF fun e) = (\e' -> t :< CallF fun e') <$> traverse go e
    go (t :< MulF l r) = (\l' r' -> t :< MulF l' r') <$> go l <*> go r

exprFuns :: Traversal (Expr v f t) (Expr v f' t) f f'
exprFuns f = go
  where
    go (t :< VarF v) = pure $ t :< VarF v
    go (t :< LitF v) = pure $ t :< LitF v
    go (t :< CallF fun e) = (\fun' e' -> t :< CallF fun' e') <$> f fun <*> traverse go e
    go (t :< MulF l r) = (\l' r' -> t :< MulF l' r') <$> go l <*> go r

statementVars :: Traversal (Statement d v f t) (Statement d v' f t) v v'
statementVars f = go
  where
    go (Declare d) = pure $ Declare d
    go (Return e) = Return <$> exprVars f e
    go (Assign v e) = Assign <$> f v <*> exprVars f e

statementFuns :: Traversal (Statement d v f t) (Statement d v f' t) f f'
statementFuns f = go
  where
    go (Declare d) = pure $ Declare d
    go (Return e) = Return <$> exprFuns f e
    go (Assign v e) = Assign v <$> exprFuns f e

statementDecls :: Traversal (Statement d v f t) (Statement d' v f t) d d'
statementDecls f = go
  where
    go (Declare d) = Declare <$> f d
    go (Return e) = pure $ Return e
    go (Assign v e) = pure $ Assign v e

functionDeclVars :: Traversal (FunctionDecl d v f fi t) (FunctionDecl d v' f fi t) v v'
functionDeclVars f (FunctionDecl a r i (Scope b)) = FunctionDecl a r i . Scope <$> (traverse . statementVars) f b

functionDeclFuns :: Traversal (FunctionDecl d v f fi t) (FunctionDecl d v f' fi t) f f'
functionDeclFuns f (FunctionDecl a r i (Scope b)) = FunctionDecl a r i . Scope <$> (traverse . statementFuns) f b

functionDeclDecls :: Traversal (FunctionDecl d v f fi t) (FunctionDecl d' v f fi t) d d'
functionDeclDecls f (FunctionDecl a r i (Scope b)) =
  (\a' b' -> FunctionDecl a' r i (Scope b')) <$> traverse f a <*> (traverse . statementDecls) f b