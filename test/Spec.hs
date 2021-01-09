{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Test.Parse
import Test.Tasty
import Test.Value

main :: IO ()
main = do
  ftests <- fileParsing
  defaultMain $
    testGroup
      "tests"
      [ simpleParsing,
        ftests,
        valueTests
      ]
