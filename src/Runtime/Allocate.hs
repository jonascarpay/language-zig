{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Runtime.Allocate where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Map
import Data.Map qualified as M
import Runtime.AST
import Runtime.Value

type Offset = Int

type FrameEnv decl = Map decl Int

-- TODO vars can also be global, decls cannot

-- declaration-order traversal of declarations in a stack frame
-- TODO handle branches? using alternative?
-- TODO handle scopes? using lift?
-- TODO better split between args and decls
-- TODO generalize traversals
traverseDeclsOrdered ::
  forall m decl decl' var fun info t.
  Applicative m =>
  (decl -> m decl') ->
  (decl -> m decl') ->
  FunctionDecl decl var fun info t ->
  m (FunctionDecl decl' var fun info t)
traverseDeclsOrdered margs mdecl (FunctionDecl args ret info (Scope b)) =
  liftA2
    (\args' b' -> FunctionDecl args' ret info (Scope b'))
    (traverse margs args)
    (traverseBlock b)
  where
    -- This is currently equivalent to a normal traversal, so we could actually drop the list,
    -- but once we do branching we need more complicated logic
    traverseBlock :: [Statement decl var fun t] -> m [Statement decl' var fun t]
    traverseBlock [] = pure []
    traverseBlock (Declare d : ss) = (\d' ss' -> Declare d' : ss') <$> mdecl d <*> traverseBlock ss
    traverseBlock (Assign v e : ss) = (\ss' -> Assign v e : ss') <$> traverseBlock ss
    traverseBlock (Return e : ss) = (\ss' -> Return e : ss') <$> traverseBlock ss

newtype AllocError name = UnknownReference name

data FrameInfo symbol = FrameInfo
  { stackBottom :: Word, -- Size of allocations below 0
    stackTop :: Word, -- Size of allocations above 0
    stackLayout :: Map symbol Offset
  }

frameSize :: FrameInfo sym -> Word
frameSize (FrameInfo bot top _) = top + bot

-- | Default allocator
-- Function arguments go at the bottom, local declarations at the top.
-- The bottom is the more negative address.
allocate ::
  forall decl var fun info t.
  Ord decl =>
  -- | Size in bytes of an allocation
  (decl -> Word) ->
  -- | Buffer below the base
  Word ->
  -- | Buffer above the base
  Word ->
  FunctionDecl decl var fun info t ->
  FrameInfo decl
allocate fBytes bot0 top0 fun = execState (traverseDeclsOrdered registerArg registerLoc fun) (FrameInfo bot0 top0 mempty)
  where
    registerArg :: decl -> State (FrameInfo decl) ()
    registerArg decl = modify $ \(FrameInfo b t l) -> FrameInfo (b + fBytes decl) t (M.insert decl (fromIntegral $ - b) l)
    registerLoc :: decl -> State (FrameInfo decl) ()
    registerLoc decl = modify $ \(FrameInfo b t l) -> let t' = t + fBytes decl in FrameInfo b t' (M.insert decl (fromIntegral t') l)

-- more postive
--   Arguments
--     hi 0+w lo
--     lo 0         (TODO shift by size of ret addr)
--   Return address (TODO, open question)
--   old ebp <- EBP
--   the rest
-- less positive

-- push arguments
-- push ret addr
-- push ebp
-- set ebp to esp

-- allocate :: Allocator -> Program name
