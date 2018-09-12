{-# LANGUAGE OverloadedStrings #-}

module TestDelphiWriter ( writerTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiWriter

import Data.Maybe (Maybe(Just))

writerTests :: TestTree
writerTests = testGroup
  "Delphi ShowDelphi tests"
  [ testGroup
      "Field Tests"
      [ testCase "Function"
      $ ("class function foo(bar: baz): fuux;" @=?)
      $ showDelphi
      $ Function (Type "foo")
                 [Arg NormalArg "bar" (Just $ Type "baz") Nothing]
                 (Type "fuux")
                 [Static]
      , testCase "Overridden Procedure"
      $ ("procedure foo(bar: baz); override;" @=?)
      $ showDelphi
      $ Procedure (Type "foo")
                  [Arg NormalArg "bar" (Just $ Type "baz") Nothing]
                  [Override]
      , testCase "Overridden constructor"
      $ ("constructor Create(bar: baz); override;" @=?)
      $ showDelphi
      $ Constructor (Type "Create")
                  [Arg NormalArg "bar" (Just $ Type "baz") Nothing]
                  [Override]
      ]
  ]
