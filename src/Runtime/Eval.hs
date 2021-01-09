{-# LANGUAGE FlexibleContexts #-}

module Runtime.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Void
import Data.Word
import Runtime.AST
import Runtime.Allocate
import Runtime.Value

data VM addr m = VM
  { readFunction :: addr -> m (FunctionDecl () () () (FrameInfo ()) Type),
    readByte :: addr -> m Word8,
    writeByte :: Word8 -> addr -> m (),
    pushBytes :: Bytes -> m addr,
    frame :: m () -> m ()
  }

type VMT name addr m a = ReaderT (VM addr m) m a

call :: name -> VMT name addr m (Value addr)
call name = undefined

evalExpr :: MonadError String m => Expr v f t -> m (Value addr)
evalExpr = go
  where
    go (t :< MulF l r) = do
      l' <- go l
      r' <- go r
      case (l', r') of
        (VU8 ul, VU8 ur) -> pure $ VU8 (ul * ur)
        _ -> throwError "cannot multiply this"
    go (t :< LitF v) = pure $ absurd <$> v
    go (t :< VarF var) = undefined
    go (t :< CallF fun args) = undefined
