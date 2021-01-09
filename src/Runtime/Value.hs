{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Runtime.Value where

import Control.Monad.Except
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
  deriving (Functor, Foldable, Traversable)

-- Any pointer is a void pointer
toType :: Value a -> Type
toType (VU8 _) = TU8
toType VVoid = TVoid
toType (VPtr _) = TPtr TVoid

typeBits :: Int -> Type -> Int
typeBits _ TU8 = 8
typeBits _ TVoid = 0
typeBits addr (TPtr _) = addr
typeBits addr (TFunPtr _) = addr

typeBytes :: Int -> Type -> Int
typeBytes addr t = div (typeBits addr t + 7) 8

newtype Bytes = Bytes {unBytes :: U.Vector Word8}

toBytes :: (addr -> Bytes) -> Value addr -> Bytes
toBytes _ (VU8 n) = Bytes $ U.singleton n
toBytes _ VVoid = Bytes U.empty
toBytes f (VPtr addr) = f addr

fromBytes ::
  (Bytes -> Either DeserializeError addr) ->
  Int ->
  Type ->
  Bytes ->
  Either DeserializeError (Value addr)
fromBytes _ addr t (Bytes bs)
  | typeBytes addr t /= U.length bs =
    throwError $ ByteLengthMismatch t (typeBytes addr t) (U.length bs)
fromBytes _ _ TVoid _ = pure VVoid
fromBytes _ _ TU8 (Bytes bs) = pure $ VU8 (U.head bs)
fromBytes f _ (TPtr _) bs = VPtr <$> f bs
fromBytes f _ (TFunPtr _) bs = VPtr <$> f bs

data DeserializeError = ByteLengthMismatch Type Int Int
