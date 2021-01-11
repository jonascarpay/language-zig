{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Runtime.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as M
import Data.Vector.Unboxed qualified as U
import Data.Void
import Data.Word
import Runtime.AST
import Runtime.Allocate
import Runtime.Value

-- VM interface
-- TODO inline var env into AST rather than keep in frameinfo
-- TODO Use type class for addrSize and conversion stuff
-- TODO Maybe even for offsetting
data VM addr var info t m = VM
  { vmReadFunction :: addr -> m (FunctionDecl () var addr info t),
    vmReadByte :: addr -> m Word8,
    vmWriteByte :: Word8 -> addr -> m (),
    vmReadEbp :: m addr,
    vmWriteEbp :: addr -> m (),
    vmFrame :: m () -> m (),
    -- vmPushBytes :: Bytes -> m addr,
    vmOffsetPtr :: addr -> Offset -> addr,
    vmAddrSize :: Int,
    vmAddrEncode :: addr -> Bytes,
    vmAddrParse :: Bytes -> addr,
    vmReadType :: t -> Type
  }

type VMT addr var info t m a =
  ExceptT
    (VMError var addr)
    (ReaderT (VM addr var info t m) m)
    a

data VMError var addr = TypeError (Value addr) (Value addr)

liftVM :: Monad m => m r -> VMT a v i t m r
liftVM = lift . lift

readValue :: Monad m => t -> addr -> VMT addr var info t m (Value addr)
readValue t' base = do
  VM {..} <- ask
  let t = vmReadType t'
  let n = typeBytes vmAddrSize t
  bs <- liftVM $ U.generateM n $ vmReadByte . vmOffsetPtr base
  pure $ fromBytes vmAddrParse t (Bytes bs)

writeValue :: Monad m => Value addr -> addr -> VMT addr var info t m ()
writeValue v base = do
  VM {..} <- ask
  let Bytes bytes = toBytes vmAddrEncode v
  liftVM $ flip U.imapM_ bytes $ \off b -> vmWriteByte b (vmOffsetPtr base off)

readLocalValue :: Monad m => t -> Offset -> VMT a v i t m (Value a)
readLocalValue t off = do
  VM {..} <- ask
  ebp <- liftVM vmReadEbp
  readValue t (vmOffsetPtr ebp off)

-- TODO addr should be fun
frame ::
  [Value addr] ->
  i ->
  ((v -> VMT addr v i t m addr) -> VMT addr v i t m r) ->
  VMT addr v i t m r
frame = undefined

call :: Monad m => [Value addr] -> addr -> VMT addr v i t m (Value addr)
call args addr = do
  VM {..} <- ask
  FunctionDecl {..} <- liftVM $ vmReadFunction addr
  frame args funInfo $ \getVar -> evalStatement getVar (unScope funBody)

evalStatement ::
  Monad m =>
  (v -> VMT addr v i t m addr) ->
  [Statement decl v addr t] ->
  VMT addr v i t m (Value addr)
evalStatement getVar = go
  where
    go [] = pure VVoid
    go (Return e : _) = evalExpr getVar e
    go (Declare _ : k) = go k
    go (Assign var e : k) = do
      val <- evalExpr getVar e
      addr <- getVar var
      writeValue val addr
      go k

-- TODO after type checking, types only matter when reading values?
-- If so, just put them in the var?
evalExpr ::
  Monad m =>
  (v -> VMT addr v i t m addr) ->
  Expr v addr t ->
  VMT addr v i t m (Value addr)
evalExpr getVar = go
  where
    go (t :< MulF l r) = do
      l' <- go l
      r' <- go r
      case (l', r') of
        (VU8 ul, VU8 ur) -> pure $ VU8 (ul * ur)
        (a, b) -> throwError $ TypeError a b
    go (t :< LitF v) = pure $ absurd <$> v
    go (t :< VarF var) = getVar var >>= readValue t
    go (t :< CallF addr args) = do
      args' <- traverse go args
      call args' addr
