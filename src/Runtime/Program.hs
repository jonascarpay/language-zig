{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Runtime.Program where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word

newtype Name = Name String deriving (Eq, Ord)

newtype Value = Value {unValue :: UV.Vector Word8}
  deriving (Ord, Eq, Show)

data Cofree f a = (:<) {extract :: a, unwrap :: f (Cofree f a)}
  deriving (Functor, Foldable, Traversable)

type a :< f = Cofree f a

type AST = Type :< ASTF

data TopLevel t
  = Function [(Name, Type)] Type (t :< ASTF)
  | Global Type (Maybe Value)
  deriving (Functor, Foldable, Traversable)

tlType :: TopLevel t -> Type
tlType (Function args t _) = TFunction (snd <$> args) t
tlType (Global t _) = t

data GlobalDecl = GlobalDecl Type (Maybe Value)

newtype Program t = Program (Map Name (TopLevel t))

data Type
  = TFunction [Type] Type
  | TPointer Type
  | TVoid
  | TU8
  deriving (Eq)

data ASTF f
  = Var Name
  | Mul f f
  | Block (NonEmpty f)
  | Return f
  | Bind Name Type (Maybe f)
  | Assign Name f
  | Call Name [f]
  deriving (Functor, Foldable, Traversable)

type Env = Map Name Type

data TypeError
  = UndefinedReference Name
  | NotAFunction
  | NameCollision Name Type Type
  | ArgumentLengthMismatch
  | Mismatch

mergeEnvs :: Env -> Env -> Typecheck Env
mergeEnvs a b =
  case M.toList $ M.intersectionWith (,) a b of
    [] -> pure $ a <> b
    ((n, (ta, tb)) : _) -> throwError $ NameCollision n ta tb

typecheck :: Program () -> Either TypeError (Program Type)
typecheck (Program prog) = Program <$> traverse f prog
  where
    f (Global t v) = pure $ Global t v
    f (Function args ret c) = Function args ret <$> evalStateT (check c) (env0 <> argenv)
      where
        env0 = tlType <$> prog
        argenv = M.fromList args

type Typecheck a = StateT Env (Either TypeError) a

tlookup :: Name -> Typecheck Type
tlookup name = gets (M.lookup name) >>= maybe (throwError $ UndefinedReference name) pure

unify :: MonadError TypeError m => Type -> Type -> m Type
unify a b = if a == b then pure a else throwError Mismatch

forkWith :: Functor m => (s -> s') -> StateT s' m a -> StateT s m a
forkWith f (StateT m) = StateT $ \s -> (fmap . fmap) (const s) (m (f s))

fork :: Functor m => StateT s m a -> StateT s m a
fork = forkWith id

check :: () :< ASTF -> Typecheck AST
check (_ :< Var name) = (:< Var name) <$> tlookup name
check (_ :< Mul a b) = do
  ta <- check a
  tb <- check b
  (:< Mul ta tb) <$> unify (extract ta) (extract tb)
check (_ :< Block cs) = fork $ do
  cs' <- traverse check cs
  pure $ TVoid :< Block cs'
check (_ :< Return f) = (\f' -> extract f' :< Return f') <$> check f
check (_ :< Bind name t mv) =
  gets (M.lookup name) >>= \case
    Nothing -> do
      mv' <- forM mv $ \v -> do
        v' <- check v
        unify (extract v') t
        pure v'
      modify (M.insert name t)
      pure $ TVoid :< Bind name t mv'
    Just t' -> throwError $ NameCollision name t t'
check (_ :< Assign name f) = (\f' -> extract f' :< Assign name f') <$> check f
check (_ :< Call name params) =
  tlookup name >>= \case
    TFunction args ret | length args == length params -> do
      let unifyArg a p = check p >>= \(t :< p') -> (:< p') <$> unify a t
      params' <- zipWithM unifyArg args params
      pure $ ret :< Call name params'
    TFunction _ _ -> throwError ArgumentLengthMismatch
    _ -> throwError NotAFunction
