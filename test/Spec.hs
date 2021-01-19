{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import Test.Eval
import Test.Parse
import Test.Tasty
import Test.Tasty.Focus

main :: IO ()
main = do
  ftests <- fileParsing
  defaultMain . withFocus $
    testGroup
      "tests"
      [ simpleParsing,
        ftests,
        focus evalTests
      ]
