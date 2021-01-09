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

data FrameInfo name = FrameInfo
  { stackBottom :: Offset, -- more positive
    stackTop :: Offset, -- less positive
    stackLayout :: Map name Offset
  }

-- TODO better interface (flexibility in ret addr, ebp, etc)
allocate ::
  forall decl var fun info t name.
  Ord name =>
  (decl -> Int) ->
  (decl -> name) ->
  FunctionDecl decl var fun info t ->
  FrameInfo name
allocate fWidth fdecl fun = execState (traverseDeclsOrdered pushBot pushTop fun) (FrameInfo 0 0 mempty)
  where
    pushBot :: decl -> State (FrameInfo name) (decl, Offset)
    pushBot decl = state $ \(FrameInfo b t env) -> ((decl, b), FrameInfo (b + fWidth decl) t (M.insert (fdecl decl) b env))
    pushTop :: decl -> State (FrameInfo name) (decl, Offset)
    pushTop decl = state $ \(FrameInfo b t env) -> let off = t + fWidth decl in ((decl, off), FrameInfo b off (M.insert (fdecl decl) off env))

-- -- TODO don't/separately traverse  vars, they're only being substituted here
-- allocate ::
--   forall decl var fun t name.
--   Ord name =>
--   (decl -> Int) ->
--   (decl -> name) ->
--   (var -> name) ->
--   FunctionDecl decl var fun t ->
--   Either (AllocError name) (FunctionDecl (decl, Offset) (var, Offset) fun t)
-- allocate fWidth fdecl fvar fun = evalStateT (traverseVars pushBot pushTop getVar fun) (AllocState 0 0 mempty)
--   where
--     pushBot :: decl -> StateT (AllocState name) (Either (AllocError name)) (decl, Offset)
--     pushBot decl = state $ \(AllocState b t env) -> ((decl, b), AllocState (b + fWidth decl) t (M.insert (fdecl decl) b env))
--     pushTop :: decl -> StateT (AllocState name) (Either (AllocError name)) (decl, Offset)
--     pushTop decl = state $ \(AllocState b t env) -> let off = t + fWidth decl in ((decl, off), AllocState b off (M.insert (fdecl decl) off env))
--     getVar :: var -> StateT (AllocState name) (Either (AllocError name)) (var, Offset)
--     getVar var =
--       gets stackEnv >>= \env -> case M.lookup name env of
--         Nothing -> throwError $ UnknownReference name
--         Just off -> pure (var, off)
--       where
--         name = fvar var

-- allocFn :: forall decl var fun t. Ord decl => (Type -> Int) -> FunctionDecl decl var fun t -> FunctionDecl Offset Offset fun t
-- allocFn f (FunctionDecl args _ (Block b)) = FunctionDecl  go (env0 mempty 0 args) 0 b
--   where
--     env0 :: -> Int -> [(decl, Type)] -> State (FrameEnv decl) Map decl Int
--     env0 e _ [] = e
--     env0 e off ((v, t) : as) = env0 (M.insert v off e) (off + f t) as
--     go :: State FrameEnv (Block Offset Offset fun t)
--     go = undefined

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
