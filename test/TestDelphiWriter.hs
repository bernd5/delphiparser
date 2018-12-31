{-# LANGUAGE OverloadedStrings #-}

module TestDelphiWriter ( writerTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiWriter

import Data.Maybe (Maybe(Just))

typ a = Type $ Lexeme "" a
arg a b c = Arg a (Lexeme "" b) (typ <$> c)

writerTests :: TestTree
writerTests = testGroup
  "Delphi ShowDelphi tests"
  [ testGroup
      "Field Tests"
      [ testCase "Function"
      $ ("class function foo(bar: baz): fuux;" @=?)
      $ showDelphi
      $ Function (typ "foo")
                 [arg NormalArg "bar" (Just $ "baz") Nothing]
                 (typ "fuux")
                 [Static]
      , testCase "Overridden Procedure"
      $ ("procedure foo(bar: baz); override;" @=?)
      $ showDelphi
      $ Procedure (typ "foo")
                  [arg NormalArg "bar" (Just $ "baz") Nothing]
                  [Override]
      , testCase "Overridden constructor"
      $ ("constructor Create(bar: baz); override;" @=?)
      $ showDelphi
      $ Constructor (typ "Create")
                  [arg NormalArg "bar" (Just $ "baz") Nothing]
                  [Override]
      ]
  ]
