{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Runtime.Value where

import Data.Bits
import Data.Proxy
import Data.Vector.Unboxed qualified as U
import Data.Word

data FunctionType = FunctionType [Type] Type
  deriving (Eq, Show)

-- It may be tempting to join Type and Value, since the constructors obviously overlap.
-- I've tried this before, and have come to the conclusion that at least for now, there is little benefit to doing so.
-- The reason is that you would probably define pattern synonyms for all cases anyway to avoid always typing Proxy and Identity.
-- This would entail a lot of trouble in and of itself, and be just as much effort as just having two separate data types.
-- Furthermore, I haven't been able to think of a good way to make actually useful conversion functions between the two.
-- It might be nice though.
-- Especially if we can carry around some proofs that make conversions easier

data Type
  = TU8
  | TVoid
  | TPtr Type
  | TFunPtr FunctionType
  deriving (Eq, Show)

data Value addr
  = VU8 Word8
  | VVoid
  | VPtr addr
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Any pointer is a void pointer
toType :: Value addr -> Type
toType (VU8 _) = TU8
toType VVoid = TVoid
toType (VPtr _) = TPtr TVoid

typeBytes :: Int -> Type -> Int
typeBytes _ TU8 = 8
typeBytes _ TVoid = 0
typeBytes n (TPtr _) = n
typeBytes n (TFunPtr _) = n

newtype Bytes = Bytes {unBytes :: U.Vector Word8}

-- | Class of types for which all values have the same number of bytes.
--   Bytes is little-endian.
--   toBytes/fromBytes roundtrips
--   TODO test this
class FixedBytes a where
  byteSize :: Proxy a -> Int
  default byteSize :: FiniteBits a => Proxy a -> Int
  byteSize _ =
    let bits = finiteBitSize (error "byteSize: finiteBitSize evaluated argument" :: a)
     in div (bits + 7) 8
  {-# INLINE byteSize #-}

  toBytes :: a -> Bytes
  default toBytes :: (FiniteBits a, Integral a) => a -> Bytes
  toBytes a =
    let n = byteSize (Proxy :: Proxy a)
     in Bytes $ U.generate n $ \i -> fromIntegral (shiftR a (i * 8))
  {-# INLINE toBytes #-}

  -- | implementations are allowed to error if there are not enough/too many
  --   bytes, but it is probably better to assume 0-padding.
  fromBytes :: Bytes -> a
  default fromBytes :: (FiniteBits a, Integral a) => Bytes -> a
  fromBytes (Bytes bs) = U.ifoldr (\i b a -> a .&. shiftL (fromIntegral b) (i * 8)) zeroBits bs
  {-# INLINE fromBytes #-}

instance FixedBytes Word8 where
  byteSize _ = 1
  toBytes = Bytes . U.singleton
  fromBytes (Bytes bs) = U.head bs

instance FixedBytes Word

instance FixedBytes () where
  byteSize _ = 0
  toBytes _ = Bytes U.empty
  fromBytes _ = ()

encodeValue :: FixedBytes addr => Value addr -> Bytes
encodeValue (VU8 n) = toBytes n
encodeValue VVoid = toBytes ()
encodeValue (VPtr addr) = toBytes addr

decodeValue :: FixedBytes addr => Type -> Bytes -> Value addr
decodeValue TVoid _ = VVoid
decodeValue TU8 bs = VU8 $ fromBytes bs
decodeValue (TPtr _) bs = VPtr $ fromBytes bs
decodeValue (TFunPtr _) bs = VPtr $ fromBytes bs

{-# INLINE writeBytes #-}
writeBytes :: Monad m => (Word8 -> Int -> m ()) -> Bytes -> Int -> m ()
writeBytes fbyte (Bytes bytes) base = flip U.imapM_ bytes $ \off byte -> fbyte byte (base + off)

{-# INLINE writeValue #-}
writeValue :: (FixedBytes x, Monad m) => (Word8 -> Int -> m ()) -> x -> Int -> m ()
writeValue fbyte = writeBytes fbyte . toBytes

{-# INLINE readBytes #-}
readBytes :: Monad m => (Int -> m Word8) -> Int -> Int -> m Bytes
readBytes fbyte n base = fmap Bytes . U.generateM n $ \off -> fbyte (base + off)

{-# INLINE readValue #-}
readValue :: forall m x. (FixedBytes x, Monad m) => (Int -> m Word8) -> Int -> m x
readValue fbyte = fmap fromBytes . readBytes fbyte (byteSize (Proxy :: Proxy x))
