{-# LANGUAGE OverloadedStrings #-}

module TestDelphiTry ( delphiTryTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (statement, delphiTry')

import DelphiAst

delphiTryTests :: TestTree
delphiTryTests = testGroup "Delphi try/finally/except Tests"
  [ testCase "try finally" $
    (Right (Try [ExpressionValue (V "foo"),ExpressionValue (V "bar")] (Right [ExpressionValue (V "baz"),ExpressionValue (V "fuux")])) @=? ) $
    parse delphiTry' "" "try\nfoo;\nbar;\nfinally\nbaz;\nfuux;\nend"
  , testCase "try finally - statement" $
    (Right (Try [ExpressionValue (V "foo"),ExpressionValue (V "bar")] (Right [ExpressionValue (V "baz"),ExpressionValue (V "fuux")])) @=? ) $
    parse statement "" "try\nfoo;\nbar;\nfinally\nbaz;\nfuux;\nend"
  ]
