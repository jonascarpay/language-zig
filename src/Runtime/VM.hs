{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.VM where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.ST
import Data.Map (Map)
import Data.STRef
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import Runtime.Program
import Runtime.Value

data VMState s = VMState
  { programMemory :: V.Vector (FunctionDecl Type),
    stackMemory :: UM.STVector s Word8,
    esp :: STRef s Word,
    ebp :: STRef s Word
  }

data VMError
  = NotAFunctionAddr Address
  | OOBRead
  | OOBWrite
  | SegFault

type Env = Map Name Address

-- Naturally, Env goes in the StateT and VMState goes in the ReaderT
type STVM s a =
  ExceptT
    VMError
    (RWST (VMState s) () Env (ST s))
    a

liftST :: ST s a -> STVM s a
liftST = lift . lift

type Address = Word

readFunction :: Address -> STVM s (FunctionDecl Type)
readFunction addr = do
  pmem <- asks programMemory
  case pmem V.!? fromIntegral addr of
    Nothing -> throwError $ NotAFunctionAddr addr
    Just f -> pure f

unstack :: Address -> Int
unstack addr = fromIntegral (maxBound - addr)

-- TODO: proper memory mapping
-- TODO: should bytes written to the stack be in reverse order?
readBytes :: Int -> Address -> STVM s Bytes
readBytes n addr = do
  let addr' = unstack addr
  stack <- asks stackMemory
  if addr' + n > UM.length stack
    then throwError OOBRead
    else Bytes <$> liftST (U.generateM n $ \i -> UM.read stack (addr' + i))

-- TODO: see notes for readBytes
writeBytes :: Address -> Bytes -> STVM s ()
writeBytes addr (Bytes bytes) = do
  let addr' = unstack addr
  stack <- asks stackMemory
  if addr' + U.length bytes > UM.length stack
    then throwError OOBWrite
    else liftST $ U.imapM_ (\i -> UM.write stack (addr' + i)) bytes
