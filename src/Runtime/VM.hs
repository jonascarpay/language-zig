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
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import Runtime.AST
import Runtime.Allocate
import Runtime.Eval
import Runtime.Value

data STVMState s = STVMState
  { programMemory :: V.Vector FunDecl',
    stackMemory :: UM.STVector s Word8,
    esp :: STRef s Address,
    ebp :: STRef s Address
  }

interface :: VM Address Offset (FrameInfo ()) Type (STVM s)
interface =
  VM
    { vmFunction = readFunction, -- :: addr -> m (FunctionDecl Offset var addr info t),
      vmReadByte = readByte, -- :: addr -> m Word8,
      vmWriteByte = writeByte, -- :: Word8 -> addr -> m (),
      vmEbp = STVM $ asks esp >>= lift . lift . readSTRef, -- :: m addr,
      vmPushFrame = pushFrame, -- :: info -> m (),
      vmPopFrame = popFrame, -- :: m (),
      vmOffsetPtr = \base off -> base + fromIntegral off, -- :: addr -> Offset -> addr,
      vmType = id, -- :: t -> Type,
      vmVar = id -- :: var -> Offset
    }

type FunDecl' = FunctionDecl Offset Offset Address (FrameInfo ()) Type

data STVMError
  = NotAFunctionAddr Address
  | OOBRead
  | OOBWrite
  | SegFault

type Env = Map Name Address

pushFrame :: FrameInfo () -> STVM s ()
pushFrame = undefined

popFrame :: STVM s ()
popFrame = undefined

-- Naturally, Env goes in the StateT and VMState goes in the ReaderT
newtype STVM s a = STVM
  { unSTVM ::
      ExceptT
        STVMError
        (RWST (STVMState s) () Env (ST s))
        a
  }
  deriving (Functor, Applicative, Monad)

type Address = Word

readFunction :: Address -> STVM s FunDecl'
readFunction addr = STVM $ do
  pmem <- asks programMemory
  case pmem V.!? fromIntegral addr of
    Nothing -> throwError $ NotAFunctionAddr addr
    Just f -> pure f

unstack :: Address -> Int
unstack addr = fromIntegral (maxBound - addr)

-- TODO: proper memory mapping
-- TODO: should bytes written to the stack be in reverse order?
readByte :: Address -> STVM s Word8
readByte addr = STVM $ do
  let addr' = unstack addr
  stack <- asks stackMemory
  if addr' > UM.length stack
    then throwError OOBRead
    else UM.read stack $ fromIntegral addr

-- TODO: see notes for readBytes
writeByte :: Word8 -> Address -> STVM s ()
writeByte byte addr = STVM $ do
  let addr' = unstack addr
  stack <- asks stackMemory
  if addr' > UM.length stack
    then throwError OOBWrite
    else UM.write stack addr' byte
