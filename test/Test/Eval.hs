module Test.Eval where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Map
import Data.Map qualified as M
import Runtime.AST
import Runtime.Allocate
import Runtime.Eval
import Runtime.Typecheck
import Runtime.VM
import Runtime.Value
import Test.Tasty
import Test.Tasty.HUnit

ret143 :: UProgram
ret143 = Program $ M.singleton "main" main
  where
    main :: UFunctionDecl
    main = FunctionDecl [] TU8 () (Scope [Return (11 * 13)])

vmAllocate :: TProgram -> Either String (Program Offset Offset Address (FrameInfo Name) Type)
vmAllocate (Program env) = Program <$> traverse f env
  where
    functionAddresses :: Map Name Address
    functionAddresses = evalState (traverse (const $ state $ \n -> (n, succ n)) env) 0
    f ::
      FunctionDecl (Name, Type) Name Name () Type ->
      Either String (FunctionDecl Offset Offset Address (FrameInfo Name) Type)
    f decl@(FunctionDecl args ret _ (Scope body)) = do
      let info@(FrameInfo _ _ slots) = allocate (fromIntegral . typeBytes 8 . snd) fst 0 16 decl
      let lookup' err name = maybe (throwError err) pure $ M.lookup name slots
      args' <- traverse (lookup' "what" . fst) args
      body' <-
        (traverse . statementVars) (lookup' "who") body
          >>= (traverse . statementFuns) (\n -> maybe (throwError "huh") pure $ M.lookup n functionAddresses)
          >>= (traverse . statementDecls) (lookup' "why" . fst)
      -- TODO better traversal here please
      -- TODO also error messages
      pure $ FunctionDecl args' ret info (Scope body')

runProgram :: UProgram -> Either String (Value Word)
runProgram uprog = do
  tprog <- first show $ typecheck uprog
  aprog <- vmAllocate tprog
  let (vprog, symbols) = compile aprog
  mainAddr <- maybe (throwError "no main") pure $ M.lookup "main" symbols
  ret <- first show $ runST $ runExceptT $ runSTVM vprog $ runEval interface $ call [] mainAddr
  first show ret

evalTests :: TestTree
evalTests =
  testGroup
    "Eval"
    [ testCase "143" $ runProgram ret143 @?= Right (VU8 143)
    ]
