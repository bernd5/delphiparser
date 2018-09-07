{-# LANGUAGE OverloadedStrings #-}

module TestDelphiFunctions ( functionTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (expression')

import DelphiAst

functionTests :: TestTree
functionTests = testGroup
  "Delphi Function Expression Tests"
  [ testCase "Very simple: foo(bar)" $ (Right (V "foo" :$ [V "bar"]) @=?) $ parse
      expression'
      ""
      "foo(bar)"
  , testCase "Very simple: foo()" $ (Right (V "foo" :$ []) @=?) $ parse
      expression'
      ""
      "foo()"
  , testCase "Very simple: foo" $ (Right (V "foo") @=?) $ parse
      expression'
      ""
      "foo"
  , testCase "Cast: string(foo)" $ (Right (V "string" :$ [V "foo"]) @=?) $ parse
      expression'
      ""
      "string(foo)"
  ]
