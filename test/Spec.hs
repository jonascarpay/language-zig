{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as CS
import Grammar
import NeatInterpolation
import Parser
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
        [ testGroup "success" testSuccess
        , testGroup "failure" testFailure
        ]
 where
  testSuccess = fmap (\(n, s) -> testCase n $ testParse p s) success
  testFailure = fmap (\(n, s) -> testCase n $ testParseFail p s) failures

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ testGroup
          "tokenization"
          [ testParser
              "token"
              (keyword "token" <* eof)
              [ ("naked", "token")
              , ("trailing space", "token   ")
              , ("trailing comment", "token // hurr")
              , ("newline comment", "token\n// hurr")
              ]
              [ ("empty", "")
              , ("leading comment", "// hurr\ntoken")
              , ("leading space", "   token")
              , ("two words", "token token")
              ]
          , testParser
              "two identifier"
              ((,) <$> keyword "a" <*> keyword "b" <* eof)
              [ ("naked", "a b")
              , ("separated by newline", "a\nb")
              , ("separated by tab", "a\tb")
              , ("separated by comment", "a// comment\nb")
              ]
              [ ("unspaced", "ab")
              , ("three tokens", "a b c")
              ]
          ]
      , testParser
          "word"
          word
          [ ("naked", "word")
          , ("leading _", "_word")
          , ("capitalized", "Word")
          , ("digits", "w0rd")
          ]
          [ ("empty", "")
          , ("leading digits", "0word")
          , ("leading @", "@word")
          ]
      , testParser
          "identifier"
          pIdentifier
          [ ("normal", "normal")
          , ("nonleading digits", "d1g1ts_")
          , ("leading @", "@ident")
          ]
          [ ("keyword", "comptime")
          , ("leading digit", "0token")
          ]
      , testParser
          "root"
          pRoot
          [
            ( "hello world"
            , [trimming|
                pub fn main() void {
                    print("heyyyyy", .{});
                }" )
              |]
            )
          ]
          []
      ]
