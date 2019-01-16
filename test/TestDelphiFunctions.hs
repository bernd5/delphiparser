{-# LANGUAGE OverloadedStrings #-}

module TestDelphiFunctions ( functionTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (expression')

import DelphiAst

v a = V (Lexeme Empty a)

functionTests :: TestTree
functionTests = testGroup
  "Delphi Function Expression Tests"
  [ testCase "Very simple: foo(bar)"
  $ (Right (v "foo" :$ [v "bar"]) @=?)
  $ parse expression' "" "foo(bar)"
  , testCase "Very simple: foo()"
  $ (Right (v "foo" :$ []) @=?)
  $ parse expression' "" "foo()"
  , testCase "Very simple: foo"
  $ (Right (v "foo") @=?)
  $ parse expression' "" "foo"
  , testCase "Cast: string(foo)"
  $ (Right (v "string" :$ [v "foo"]) @=?)
  $ parse expression' "" "string(foo)"
  ]
