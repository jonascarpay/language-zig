{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Runtime.Typecheck (typecheck, TypeError) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Prettyprinter
import Runtime.AST
import Runtime.Value

type Env = Map Name Type

data TypeError
  = NameCollision Name Type Type
  | EndOfNonVoidFunction
  | NoSuchVar Name
  | NotAFunction Name Type
  | NoSuchFun Name
  | UnreachableCode
  | Ununifiable
  | ArgumentLengthMismatch
  deriving (Show)

type Typecheck a = StateT Env (Either TypeError) a

instance Pretty TypeError

typecheck :: UProgram -> Either TypeError TProgram
typecheck (Program prg) = Program <$> traverse (typecheckFn env0) prg
  where
    env0 = TFunPtr . funType snd <$> prg

funType :: (decl -> Type) -> FunctionDecl decl var fun info t -> FunctionType
funType f (FunctionDecl a r _ _) = FunctionType (f <$> a) r

declare :: Name -> Type -> Typecheck ()
declare n t = do
  m <- get
  case M.lookup n m of
    Nothing -> put $ M.insert n t m
    Just t' -> throwError $ NameCollision n t t'

-- TODO some kind of semantics
-- TODO left/right-leaning
-- TODO addresses?
-- TODO Don't allow free recasting of (function) pointers
unify :: Type -> Type -> Typecheck Type
unify TU8 TU8 = pure TU8
unify TVoid TVoid = pure TU8
unify (TPtr t) (TPtr _) = pure $ TPtr t
unify (TFunPtr t) (TFunPtr _) = pure $ TFunPtr t
unify _ _ = throwError Ununifiable

typecheckFn :: Env -> UFunctionDecl -> Either TypeError TFunctionDecl
typecheckFn env (FunctionDecl a r info (Scope b)) = flip evalStateT env $ do
  mapM_ (uncurry declare) a
  b' <- typecheckBlock r b
  pure $ FunctionDecl a r info (Scope b')

typecheckBlock :: Type -> [UStatement] -> Typecheck [TStatement]
typecheckBlock ret = go
  where
    go :: [UStatement] -> Typecheck [TStatement]
    go (Declare (n, t) : k) = declare n t >> (Declare (n, t) :) <$> go k
    go (Assign n expr : k) = do
      expr' <- typecheckExpr expr
      (Assign n expr' :) <$> go k
    go (Return expr : k) = do
      t :< expr' <- typecheckExpr expr
      t' <- unify ret t
      if null k
        then pure [Return (t' :< expr')]
        else throwError UnreachableCode
    go []
      | ret == TVoid = pure []
      | otherwise = throwError EndOfNonVoidFunction

typecheckExpr :: UExpr -> Typecheck TExpr
typecheckExpr (() :< VarF name) = do
  gets (M.lookup name) >>= \case
    Nothing -> throwError $ NoSuchVar name
    Just t -> pure $ t :< VarF name
typecheckExpr (() :< MulF l r) = do
  l' <- typecheckExpr l
  r' <- typecheckExpr r
  t <- unify (extract l') (extract r')
  pure (t :< MulF l' r')
typecheckExpr (() :< LitF v) = pure $ toType v :< LitF v
typecheckExpr (() :< CallF fun args) = do
  gets (M.lookup fun) >>= \case
    Just (TFunPtr (FunctionType argTypes ret)) -> do
      when (length args /= length argTypes) $ throwError ArgumentLengthMismatch
      args' <- forM (zip args argTypes) $ \(expr, t) -> do
        -- TODO some kind of casting
        _ :< ee <- typecheckExpr expr
        pure $ t :< ee
      pure $ ret :< CallF fun args'
    Just t -> throwError $ NotAFunction fun t
    Nothing -> throwError $ NoSuchFun fun
