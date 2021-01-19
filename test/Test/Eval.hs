{-# LANGUAGE OverloadedStrings #-}

module Test.Eval where

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

{-# ANN test143 ("HLINT: ignore Redundant $" :: String) #-} -- allows mixed lininess from ormolu
test143 :: TestTree
test143 =
  testGroup
    "returns 143"
    [ testGroup
        "simple"
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
            ]
        ],
      testGroup
        "variables"
        [ mkCase
            "declare unused variable"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  Return 143
                ]
            ],
          mkCase
            "declare and assign unused variable"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= 99,
                  Return 143
                ]
            ],
          mkCase
            "declare and reassign unused variable"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= 99,
                  "y" .: TU8,
                  "y" .= 10 * "x",
                  Return 143
                ]
            ],
          mkCase
            "declare and return variable"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= 143,
                  -- "y" .: TU8, -- FIXME adding a non-void declaration fixes the test, indicating that we can't read from top of stack
                  Return "x"
                ]
            ]
        ],
      testGroup
        "functions"
        [ mkCase
            "call second function"
            [ decl "main" [] TU8 [Return $ Call "ret143" []],
              decl "ret143" [] TU8 [Return 143]
            ],
          mkCase
            "call second function with unused argument"
            [ decl "main" [] TU8 [Return $ Call "ret143" [99]],
              decl "ret143" [("_", TU8)] TU8 [Return 143]
            ],
          mkCase
            "return result of identity function"
            [ decl "main" [] TU8 [Return $ Call "id" [143]],
              decl "id" [("x", TU8)] TU8 [Return "x"]
            ],
          mkCase
            "return result of multiplication function"
            [ decl "main" [] TU8 [Return $ Call "id" [11, 13]],
              decl "id" [("x", TU8), ("y", TU8)] TU8 [Return $ "x" * "y"]
            ]
        ]
    ]
  where
    mkCase :: String -> [(String, UFunctionDecl)] -> TestTree
    mkCase name funs =
      testCase name $
        let program = Program $ M.fromList funs
         in case runProgram program of
              Right r -> r @?= VU8 143
              Left err -> assertFailure err
    decl :: String -> [(Name, Type)] -> Type -> [UStatement] -> (String, UFunctionDecl)
    decl name args ret body = (name, FunctionDecl args ret () (Scope body))
    (.:) :: String -> Type -> UStatement
    (.:) name t = Declare (name, t)
    infix 4 .=
    (.=) :: String -> UExpr -> UStatement
    (.=) name x = Assign name x

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
evalTests = testGroup "Evaluation tests" [test143]
