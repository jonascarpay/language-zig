{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CS
import Grammar
import Parsing
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

testParse :: Parser a -> ByteString -> IO ()
testParse parser str =
  case parse (parser <* eof) "" str of
    Left e -> assertFailure (errorBundlePretty e)
    Right _ -> pure ()

testParseFail :: Show a => Parser a -> ByteString -> IO ()
testParseFail parser str =
  case parse (parser <* eof) "" str of
    Left _ -> pure ()
    Right e -> assertFailure $ "Unexpectedly succeeded parsing: " <> show e

testParser ::
  Show a =>
  String ->
  Parser a ->
  [(String, ByteString)] ->
  [(String, ByteString)] ->
  TestTree
testParser group p success failures =
  testGroup
    group
    $ if null success || null failures
      then testSuccess <> testFailure
      else
        [ testGroup "success" testSuccess,
          testGroup "failure" testFailure
        ]
  where
    testSuccess = fmap (\(n, s) -> testCase n $ testParse p s) success
    testFailure = fmap (\(n, s) -> testCase n $ testParseFail p s) failures

main :: IO ()
main =
  defaultMain $
    testGroup
      "tokenization"
      [ testParser
          "single identifier"
          (identifier *> eof)
          [ ("naked", "token"),
            ("trailing space", "token   "),
            ("trailing comment", "token // hurr")
          ]
          [ ("empty", ""),
            ("leading space", "   token"),
            ("leading digit", "0token")
          ],
        testParser
          "two identifiers"
          (identifier *> identifier *> eof)
          [ ("naked", "token1 token2"),
            ("separated by newline", "a\nb"),
            ("separated by tab", "a\tb"),
            ("separated by comment", "a// comment\nb")
          ]
          [("unspaced", "ab")]
      ]
