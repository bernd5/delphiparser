{-# LANGUAGE OverloadedStrings #-}

module TestDelphiFunctions ( functionTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (expression')

import DelphiAst
import TestSupport

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
  , testCase "WriteLn(foo:4:8) - pascal special case"
  $ (Right (v "WriteLn" :$ [v "foo" :--:-- I (Lexeme NoDirective 4) :--:-- I (Lexeme NoDirective 8)]) @=?)
  $ parse expression' "" "WriteLn(foo:4:8)"
  , testCase "WriteLn(foo:2) - pascal special case"
  $ (Right (v "WriteLn" :$ [v "foo" :--:-- I(Lexeme NoDirective 4)]) @=?)
  $ parse expression' "" "WriteLn(foo:4)"
  , testCase "Write(foo)"
  $ (Right (v "Write" :$ [v "foo"]) @=?)
  $ parse expression' "" "Write(foo)"
  , testCase "Read(foo)"
  $ (Right (v "Read" :$ [v "foo"]) @=?)
  $ parse expression' "" "Read(foo)"
  ]
