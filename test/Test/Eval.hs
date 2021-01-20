{-# LANGUAGE OverloadedStrings #-}

module Test.Eval where

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Map
import Data.Map qualified as M
import Data.Text.Prettyprint.Doc
import Data.Vector qualified as V
import Prettyprinter.Render.String (renderString)
import Runtime.AST
import Runtime.Allocate
import Runtime.Eval
import Runtime.Typecheck
import Runtime.VM
import Runtime.Value
import Test.Tasty
import Test.Tasty.Focus
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
                  "y" .= 10 * "y",
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
            "nested identities"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= Call "id" [Call "id" [143]],
                  Return $ "x"
                ],
              decl "id" [("x", TU8)] TU8 [Return $ Call "id_nest" ["x"]],
              decl "id_nest" [("x", TU8)] TU8 [Return $ "x"]
            ],
          mkCase
            "return result of multiplication function"
            [ decl "main" [] TU8 [Return $ Call "mul" [11, 13]],
              decl "mul" [("x", TU8), ("y", TU8)] TU8 [Return $ "x" * "y"]
            ],
          mkCase
            "assign function result"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= Call "id" [13],
                  "y" .: TU8,
                  "y" .= Call "id" [11],
                  Return $ "x" * "y"
                ],
              decl "id" [("x", TU8)] TU8 [Return $ "x"]
            ],
          mkCase
            "complicate things"
            [ decl "main" [] TU8 $
                [ "x" .: TU8,
                  "x" .= Call "id" [Call "ret13" []],
                  "y" .: TU8,
                  "y" .= Call "id" [Call "id" [11]],
                  "r" .: TU8,
                  "r" .= Call "mul" [Call "id2" ["x"], "y"],
                  "y" .= "r",
                  Return $ Call "id" ["y"]
                ],
              decl "id" [("x", TU8)] TU8 [Return $ Call "id2" ["x"]],
              decl "id2" [("x", TU8)] TU8 [Return $ Call "id3" ["x"]],
              decl "id3" [("x", TU8)] TU8 [Return $ "x"],
              decl "mul" [("x", TU8), ("y", TU8)] TU8 [Return $ "x" * "y"],
              decl "ret13" [] TU8 [Return 13]
            ]
        ]
    ]
  where
    mkCase :: String -> [(String, UFunctionDecl)] -> TestTree
    mkCase name funs =
      testCase name $
        assertDoc (VU8 143) $ do
          let program = Program $ M.fromList funs
          tprog <- first pretty $ typecheck program
          aprog <- vmAllocate tprog
          let (cprog, symbols) = compile aprog
          mainAddr <- maybe (throwError "no main") pure $ M.lookup "main" symbols
          let runtimeError err =
                vsep
                  [ "Runtime error:",
                    indent 2 err,
                    "while running",
                    indent 2 $
                      vsep
                        [ "AST",
                          indent 2 $ pretty tprog,
                          "compiled",
                          indent 2 $ vsep $ pretty <$> V.toList cprog
                        ]
                  ]
          -- Left $ runtimeError mempty
          first runtimeError $ runProgram cprog mainAddr
    decl :: String -> [(Name, Type)] -> Type -> [UStatement] -> (String, UFunctionDecl)
    decl name args ret body = (name, FunctionDecl args ret () (Scope body))
    (.:) :: String -> Type -> UStatement
    (.:) name t = Declare (name, t)
    infix 4 .=
    (.=) :: String -> UExpr -> UStatement
    (.=) name x = Assign name x

assertDoc :: (Show a, Eq a) => a -> Either (Doc ann) a -> Assertion
assertDoc a (Right b) = a @=? b
assertDoc _ (Left err) = assertFailure $ renderString $ layoutSmart defaultLayoutOptions err

vmAllocate :: TProgram -> Either (Doc ann) (Program Offset Offset Address (FrameInfo Name) Type)
vmAllocate (Program env) = Program <$> traverse f env
  where
    functionAddresses :: Map Name Address
    functionAddresses = evalState (traverse (const $ state $ \n -> (n, succ n)) env) 0
    f ::
      FunctionDecl (Name, Type) Name Name () Type ->
      Either (Doc ann) (FunctionDecl Offset Offset Address (FrameInfo Name) Type)
    f decl@(FunctionDecl args ret _ (Scope body)) = do
      let info@(FrameInfo _ _ slots) = allocate (fromIntegral . typeBytes 8 . snd) fst 0 8 decl
      let lookup' err name = maybe (throwError err) pure $ M.lookup name slots
      args' <- traverse (lookup' "what" . fst) args
      body' <-
        (traverse . statementVars) (lookup' "who") body
          >>= (traverse . statementFuns) (\n -> maybe (throwError "huh") pure $ M.lookup n functionAddresses)
          >>= (traverse . statementDecls) (lookup' "why" . fst)
      -- TODO better traversal here please
      -- TODO also error messages
      pure $ FunctionDecl args' ret info (Scope body')

type CompiledProgram = V.Vector FunDecl'

compileProgram :: UProgram -> Either (Doc ann) (CompiledProgram, Address)
compileProgram uprog = do
  tprog <- first pretty $ typecheck uprog
  aprog <- vmAllocate tprog
  let (cprog, symbols) = compile aprog
  mainAddr <- maybe (throwError "no main") pure $ M.lookup "main" symbols
  pure (cprog, mainAddr)

runProgram :: CompiledProgram -> Address -> Either (Doc ann) (Value Word)
runProgram prog addr = do
  ret <- first pretty $ runST $ runExceptT $ runSTVM prog $ runEval interface $ call [] addr
  first pretty ret

evalTests :: TestTree
evalTests = testGroup "Evaluation tests" [test143]
