{-# LANGUAGE OverloadedStrings #-}

module TestWith ( withTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (with')

import DelphiAst

withTests :: TestTree
withTests = testGroup "Delphi With Tests"
  [ testCase "with foo do bar" $
    (Right EmptyExpression @=? ) $
    parse with' "" "with foo do bar"
  ]
