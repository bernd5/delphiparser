{-# LANGUAGE OverloadedStrings #-}

module TestDelphiTry ( delphiTryTests ) where

import Data.Text (unpack, intercalate)

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

  , testCase "Complex try/with/case" $
    (Right (Try [With [(V "foo")] (Case (V "foo") [CaseBranch [V "one"] (ExpressionValue (V "alpha")),CaseBranch [V "two"] (ExpressionValue (V "beta"))] (Just (Else (V "Result" := V "gamma"))))] (Left [ExceptOn Nothing [ExpressionValue (V "chalk" :$ [])]])) @=? ) $
    parse statement "" $ unpack $ intercalate "\n" 
      [ "try"
      , "with foo do"
      , "case foo of"
      , "one: alpha;"
      , "two: beta;"
      , "else"
      , "Result := gamma;"
      , "end;"
      , "except"
      , "chalk();"
      , "end;"
      ]
      
      
  ]
