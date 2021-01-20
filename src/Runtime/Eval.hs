{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy
import Data.Vector.Unboxed qualified as U
import Data.Void
import Data.Word
import Prettyprinter
import Runtime.AST
import Runtime.Allocate
import Runtime.Value

-- VM interface
-- TODO make things less polymorphic
-- TODO bulk writes? need to think about endianness
-- TODO inline var env into AST rather than keep in frameinfo
-- TODO Use type class for addrSize and conversion stuff
-- TODO Maybe even for offsetting
data VM addr var info t m = VM
  { vmFunction :: addr -> m (FunctionDecl Offset var addr info t),
    vmReadByte :: addr -> m Word8,
    vmWriteByte :: Word8 -> addr -> m (),
    vmEbp :: m addr,
    vmPushFrame :: info -> m (),
    vmPopFrame :: info -> m (),
    vmOffsetPtr :: addr -> Offset -> addr,
    vmType :: t -> Type,
    vmVar :: var -> Offset
  }

type VMT addr var info t m a =
  ExceptT
    (VMError var addr)
    (ReaderT (VM addr var info t m) m)
    a

runEval :: VM addr var info t m -> VMT addr var info t m a -> m (Either (VMError var addr) a)
runEval vm m = runReaderT (runExceptT m) vm

data VMError var addr = TypeError (Value addr) (Value addr)
  deriving (Show)

instance Pretty (VMError var addr) where
  pretty (TypeError _ _) = "vm runtime error that shouldn't happen fix this error if it does"

liftVM :: Monad m => m r -> VMT a v i t m r
liftVM = lift . lift

readValue' :: forall addr var info t m. (FixedBytes addr, Monad m) => t -> addr -> VMT addr var info t m (Value addr)
readValue' t' base = do
  VM {..} <- ask
  let t = vmType t'
  let n = typeBytes (byteSize (Proxy :: Proxy addr)) t
  bs <- liftVM $ U.generateM n $ vmReadByte . vmOffsetPtr base
  pure $ decodeValue t (Bytes bs)

writeValue' :: (FixedBytes addr, Monad m) => Value addr -> addr -> VMT addr var info t m ()
writeValue' v base = do
  VM {..} <- ask
  let Bytes bytes = encodeValue v
  liftVM $ flip U.imapM_ bytes $ \off b -> vmWriteByte b (vmOffsetPtr base off)

writeValueRev' :: (FixedBytes addr, Monad m) => Value addr -> addr -> VMT addr var info t m ()
writeValueRev' v base = do
  VM {..} <- ask
  let Bytes bytes = encodeValue v
  liftVM $ flip U.imapM_ bytes $ \off b -> vmWriteByte b (vmOffsetPtr base off)

call :: (Integral addr, Show addr, FixedBytes addr, Monad m) => [Value addr] -> addr -> VMT addr v i t m (Value addr)
call args addr = do
  VM {..} <- ask
  FunctionDecl {..} <- liftVM $ vmFunction addr
  liftVM $ vmPushFrame funInfo
  forM_ (zip args funArgs) $ \(v, off) -> readOffset off >>= writeValue' v
  r <- evalStatement (unScope funBody)
  liftVM $ vmPopFrame funInfo
  pure r

readOffset :: Monad m => Offset -> VMT a v i t m a
readOffset off = do
  base <- asks vmEbp >>= liftVM
  f <- asks vmOffsetPtr
  pure $ f base off

readVar :: Monad m => v -> VMT a v i t m a
readVar var = ask >>= readOffset . ($var) . vmVar

evalStatement ::
  (Integral addr, Show addr, FixedBytes addr, Monad m) =>
  [Statement decl v addr t] ->
  VMT addr v i t m (Value addr)
evalStatement = go
  where
    go [] = pure VVoid
    go (Return e : _) = evalExpr e
    go (Declare _ : k) = go k
    go (Assign var e : k) = do
      addr <- readVar var
      val <- evalExpr e
      writeValue' val addr
      go k

-- TODO after type checking, types only matter when reading values?
-- If so, just put them in the var?
evalExpr ::
  (Integral addr, Show addr, FixedBytes addr, Monad m) =>
  Expr v addr t ->
  VMT addr v i t m (Value addr)
evalExpr = go
  where
    go (t :< MulF l r) = do
      l' <- go l
      r' <- go r
      case (l', r') of
        (VU8 ul, VU8 ur) -> pure $ VU8 (ul * ur)
        (a, b) -> throwError $ TypeError a b
    go (t :< LitF v) = pure $ absurd <$> v
    go (t :< VarF var) = readVar var >>= readValue' t
    go (t :< CallF addr args) = do
      args' <- traverse go args
      call args' addr
