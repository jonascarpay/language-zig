{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Runtime.VM where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.ST
import Data.Map (Map)
import Data.Map qualified as M
import Data.STRef
import Data.Text.Prettyprint.Doc
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word
import Runtime.AST
import Runtime.Allocate
import Runtime.Eval
import Runtime.Value
import Text.Printf

data STVMState s = STVMState
  { programMemory :: V.Vector FunDecl',
    stackMemory :: UM.STVector s Word8,
    esp :: STRef s Address,
    ebp :: STRef s Address
  }

-- For debugging
-- TODO HKD over STVMState?
data STVMFreeze = STVMFreeze
  { frozenStack :: UV.Vector Word8,
    frozenEsp :: Address,
    frozenEbp :: Address
  }

instance Pretty STVMFreeze where
  pretty (STVMFreeze st es eb) =
    align . vsep $
      [ "stack",
        indent 2 $ showMem st,
        "esp" <+> prettyWord es <+> parens (pretty $ unstack es),
        "ebp" <+> prettyWord eb <+> parens (pretty $ unstack eb)
      ]

showMem :: UV.Vector Word8 -> Doc ann
showMem vec = align $ vsep $ go (UV.toList vec)
  where
    go [] = []
    go xs =
      let (h, t) = splitAt 8 xs
       in hsep (prettyByte <$> h) : go t

prettyByte :: Word8 -> Doc ann
prettyByte b = pretty (printf "%02x" b :: String)

-- prettyWord :: Word -> Doc ann
-- prettyWord n = pretty (printf "%016x" n :: String)
prettyWord :: Word -> Doc ann
prettyWord n = pretty (fromIntegral n :: Int)

-- For debugging
{-# ANN freezeState ("hlint: ignore" :: String) #-}
freezeState :: STVMState s -> ST s STVMFreeze
freezeState STVMState {..} = do
  fesp <- readSTRef esp
  febp <- readSTRef ebp
  fstack <- UV.freeze stackMemory
  pure $ STVMFreeze fstack fesp febp

type CheckedProgam = Program Offset Offset Address (FrameInfo Name) Type

compile :: CheckedProgam -> (V.Vector FunDecl', Map Name Address)
compile (Program prog) =
  let (names, funs) = unzip $ M.toList prog
   in (V.fromList funs, M.fromList $ zip names [0 ..])

vmState0 :: V.Vector FunDecl' -> ST s (STVMState s)
vmState0 programMemory = do
  esp <- newSTRef maxBound
  ebp <- newSTRef maxBound
  stackMemory <- UM.replicate 0x40 0
  pure $ STVMState {..}

-- TODO should function names just be addresses?
interface :: VM Address Offset (FrameInfo Name) Type (STVM s)
interface =
  VM
    { vmFunction = readFunction, -- :: addr -> m (FunctionDecl Offset var addr info t),
      vmReadByte = readByte, -- :: addr -> m Word8,
      vmWriteByte = writeByte, -- :: Word8 -> addr -> m (),
      vmEbp = STVM $ asks ebp >>= lift . lift . readSTRef, -- :: m addr,
      vmPushFrame = pushFrame, -- :: info -> m (),
      vmPopFrame = popFrame, -- :: m (),
      vmOffsetPtr = \base off -> base + fromIntegral off, -- :: addr -> Offset -> addr,
      vmType = id, -- :: t -> Type,
      vmVar = id -- :: var -> Offset
    }

type FunDecl' = FunctionDecl Offset Offset Address (FrameInfo Name) Type

runSTVM :: V.Vector FunDecl' -> STVM s a -> ExceptT (STVMError, STVMFreeze) (ST s) a
runSTVM prog (STVM m) = ExceptT $ do
  stateInit <- vmState0 prog
  (res, _, _) <- runRWST (runExceptT m) stateInit mempty
  case res of
    Left err -> (\fs -> Left (err, fs)) <$> freezeState stateInit
    Right res -> pure $ Right res

data STVMError
  = NotAFunctionAddr Address
  | OOBRead Address
  | OOBWrite Address
  | SegFault
  deriving (Eq, Show)

instance Pretty STVMError

type Env = Map Name Address

-- TODO better way to express layouts
ebpOff :: Offset
ebpOff = -7

-- TODO catch stack overflows
pushFrame :: FrameInfo decl -> STVM s ()
pushFrame (FrameInfo wbot wtop _) = STVM $ do
  ebpOld <- asks ebp >>= lift . lift . readSTRef
  espOld <- asks esp >>= lift . lift . readSTRef
  let ebpNew = espOld - wbot
      espNew = ebpNew - wtop
  -- TODO When writing ebp to the stack here, we manually offset the pointer.
  -- This is dangerous and should somehow happen automatically.
  -- TODO going through Int here does not make sense, writeValue should be reworked
  writeValue (\b i -> unSTVM $ writeByte b (fromIntegral i)) ebpOld (fromIntegral ebpNew + ebpOff)
  asks ebp >>= lift . lift . flip writeSTRef ebpNew
  asks esp >>= lift . lift . flip writeSTRef espNew

popFrame :: FrameInfo decl -> STVM s ()
popFrame (FrameInfo wbot _ _) = STVM $ do
  ebpNew <- asks ebp >>= lift . lift . readSTRef
  -- TODO Same as pushFrame, we manually offset the address we write to
  ebpOld <- readValue (unSTVM . readByte . fromIntegral) (fromIntegral ebpNew + ebpOff)
  asks esp >>= lift . lift . flip writeSTRef (ebpNew + wbot)
  asks ebp >>= lift . lift . flip writeSTRef ebpOld

-- TODO get rid of the Env?
-- TODO Moving the ExceptT inside makes composition easier since everything runs in an exceptT, but makes conserving state harder?
-- Naturally, Env goes in the StateT and VMState goes in the ReaderT
newtype STVM s a = STVM
  { unSTVM ::
      ExceptT
        STVMError
        (RWST (STVMState s) () Env (ST s))
        a
  }
  deriving (Functor, Applicative, Monad)

-- TODO newtype, show as hex
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
  if addr' < 0 || addr' >= UM.length stack
    then throwError (OOBRead addr)
    else UM.read stack addr'

-- TODO: see notes for readBytes
writeByte :: Word8 -> Address -> STVM s ()
writeByte byte addr = STVM $ do
  let addr' = unstack addr
  stack <- asks stackMemory
  if addr' < 0 || addr' >= UM.length stack
    then throwError (OOBWrite addr)
    else UM.write stack addr' byte
