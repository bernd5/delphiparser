{-# LANGUAGE OverloadedStrings #-}

module TestConst (constTests) where

import DelphiAst
import DelphiParser (constExpressions, singleConstExpression)

import Test.Tasty (TestTree, testGroup)
import TestSupport

constTests :: TestTree
constTests = testGroup "Delphi Const Tests"
  [ testCase' "foo = 3" singleConstExpression
  $ constDef "foo" 3
  , testCase' "const foo = 3; foo = 3;" constExpressions
  $ ConstDefinitions [constDef "foo" 3, constDef "foo" 3]
  ]
