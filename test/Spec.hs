{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Test.Eval
import Test.Parse
import Test.Tasty

main :: IO ()
main = do
  ftests <- fileParsing
  defaultMain $
    testGroup
      "tests"
      [ simpleParsing,
        ftests,
        evalTests
      ]
