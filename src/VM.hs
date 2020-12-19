{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VM where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Bits
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Proxy
import Data.STRef
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import Program
import Text.Printf

type Region s = UM.STVector s Word8
type Register s = STRef s Address

-- Addresses are _before_ memory mapping
-- i.e. maxBound :: Word is the base of the stack,
-- even though the backing memory for the stack
-- is not that big.
newtype Address = Address {unAddress :: Word}
  deriving (Num, Eq, Ord, Bounded)

instance Show Address where
  show (Address x) = printf "0x%x" x

toNum :: (Num n, FiniteBits n) => Value -> n
toNum (Value v) = UV.foldl' (\n b -> shiftL n 8 + fromIntegral b) 0 v

fromNum :: (FiniteBits n, Integral n) => n -> Value
fromNum x = Value $ UV.generate bytes $ \n -> fromIntegral (shiftR x ((bytes - n -1) * 8))
 where
  bytes = div (finiteBitSize x + 7) 8

data Runtime s = Runtime
  { stack :: Region s -- TODO proper growable stack?
  , ebp :: Register s
  , esp :: Register s
  , globalMem :: Region s
  , staticMem :: UV.Vector Word8
  , procMem :: V.Vector AST
  }

data RuntimeError
  = OOBRead
  | OOBWrite
  | ROMWrite
  | ProcRead

type RunEnv = Map Name Address

-- TODO unbox
data MemoryMap = MemoryMap
  { stackBase :: !Address
  , globalBase :: !Address
  , staticBase :: !Address
  , procBase :: !Address
  }

memMap :: Runtime s -> MemoryMap
memMap (Runtime stack _ _ _ static proc) =
  let stackBase = maxBound - fromIntegral (UM.length stack) + 1
      procBase = 0
      staticBase = fromIntegral (V.length proc)
      globalBase = staticBase + fromIntegral (UV.length static)
   in MemoryMap{..}

write :: Address -> Value -> VM s ()
write addr x = do
  Runtime{..} <- ask
  MemoryMap{..} <- asks memMap
  case () of
    _
      | addr >= stackBase -> writeRegion stack (unAddress $ addr - stackBase) x
      | addr >= globalBase -> writeRegion globalMem (unAddress $ addr - globalBase) x
      | otherwise -> throwError ROMWrite
 where
  writeRegion :: Region s -> Word -> Value -> VM s ()
  writeRegion region base (Value vec)
    | base >= 0 && base + bytes <= regionSize = flip UV.imapM_ vec $
      \offset byte -> UM.write region (fromIntegral base + offset) byte -- TODO unsafeWrite
    | otherwise = throwError OOBWrite
   where
    bytes = fromIntegral $ UV.length vec
    regionSize = fromIntegral $ UM.length region

read :: Address -> Word -> VM s Value
read addr nbytes = do
  Runtime{..} <- ask
  MemoryMap{..} <- asks memMap
  case () of
    _
      | addr >= stackBase -> readRegion stack (unAddress $ addr - stackBase) nbytes
      | addr >= globalBase -> readRegion globalMem (unAddress $ addr - globalBase) nbytes
      | addr >= staticBase -> slice staticMem (unAddress $ addr - staticBase) nbytes
      | otherwise -> throwError ProcRead
 where
  slice :: UV.Vector Word8 -> Word -> Word -> VM s Value
  slice vec base bytes
    | base >= 0 && base + bytes <= fromIntegral (UV.length vec) =
      pure $ Value $ UV.slice (fromIntegral base) (fromIntegral bytes) vec
    | otherwise = throwError OOBRead
  readRegion :: Region s -> Word -> Word -> VM s Value
  readRegion region base bytes
    | base >= 0 && base + bytes <= regionSize = fmap Value $
      UV.generateM (fromIntegral bytes) $
        \offset -> UM.read region (fromIntegral base + offset)
    | otherwise = throwError OOBRead
   where
    regionSize = fromIntegral $ UM.length region

-- TODO possibly optimize
push :: forall s. Value -> VM s Address
push x = do
  base' <- stackGrow bytes
  write base' x
  pure base'
 where
  bytes = fromIntegral $ UV.length $ unValue x

  -- TODO overflow check
  stackGrow :: Int -> VM s Address
  stackGrow n = do
    espR <- asks esp
    espOld <- liftST $ readSTRef espR
    let espNew = espOld - fromIntegral n
    liftST $ writeSTRef espR espNew
    pure espNew

type VM s a = ExceptT RuntimeError (StateT RunEnv (ReaderT (Runtime s) (ST s))) a
liftST :: ST s a -> VM s a
liftST = lift . lift . lift
