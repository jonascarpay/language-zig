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

test143 :: [(String, UProgram)]
test143 =
  [ mkCase
      "return immediately"
      [decl "main" [] TU8 [Return 143]],
    mkCase
      "return expression"
      [decl "main" [] TU8 [Return $ 11 * 13]],
    mkCase
      "return immediately, with unused dummy function"
      [ decl "main" [] TU8 [Return 143],
        decl "dummy" [] TVoid []
      ],
    mkCase
      "call second function"
      [ decl "main" [] TU8 [Return $ Call "ret143" []],
        decl "ret143" [] TU8 [Return 143]
      ]
  ]
  where
    mkCase :: String -> [(String, UFunctionDecl)] -> (String, UProgram)
    mkCase name funs = (name, Program $ M.fromList funs)
    decl :: String -> [(Name, Type)] -> Type -> [UStatement] -> (String, UFunctionDecl)
    decl name args ret body = (name, FunctionDecl args ret () (Scope body))

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
    "Evaluation tests"
    [ testGroup "143 tests" $
        flip fmap test143 $ \(name, prog) ->
          testCase name $ runProgram prog @=? Right (VU8 143)
    ]
