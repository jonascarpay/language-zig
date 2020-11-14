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

parseKeyword :: String -> Parser a -> TestTree
parseKeyword str p = testCase str $ testParse p (CS.pack str)

main :: IO ()
main =
  defaultMain $
    testGroup
      "simple keywords"
      [ parseKeyword "comptime" pKeywordComptime,
        parseKeyword "for" pKeywordFor
      ]
