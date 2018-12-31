{-# LANGUAGE OverloadedStrings #-}

module TestDelphiTry ( delphiTryTests ) where

import Data.Text (unpack, intercalate)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (statement, delphiTry')

import DelphiAst

v a = V $ Lexeme "" a

delphiTryTests :: TestTree
delphiTryTests = testGroup
  "Delphi try/finally/except Tests"
  [ testCase "try finally"
  $ (Right
      (Try [ExpressionValue (v "foo"), ExpressionValue (v "bar")]
           (Right [ExpressionValue (v "baz"), ExpressionValue (v "fuux")])
      ) @=?
    )
  $ parse delphiTry' "" "try\nfoo;\nbar;\nfinally\nbaz;\nfuux;\nend"
  , testCase "try finally - statement"
  $ (Right
      (Try [ExpressionValue (v "foo"), ExpressionValue (v "bar")]
           (Right [ExpressionValue (v "baz"), ExpressionValue (v "fuux")])
      ) @=?
    )
  $ parse statement "" "try\nfoo;\nbar;\nfinally\nbaz;\nfuux;\nend"
  , testCase "Complex try/with/case"
  $ (Right
      (Try
        [ With
            [(v "foo")]
            (Case
              (v "foo")
              [ CaseBranch [v "one"] (ExpressionValue (v "alpha"))
              , CaseBranch [v "two"] (ExpressionValue (v "beta"))
              ]
              (Just (Else (v "Result" := v "gamma")))
            )
        ]
        (Left [ExceptOn Nothing [ExpressionValue (v "chalk" :$ [])]])
      ) @=?
    )
  $ parse statement ""
  $ unpack
  $ intercalate
      "\n"
      [ "try"
      , "with foo do"
      , "case foo of"
      , "one: alpha;"
      , "two: beta;"
      , "else"
      , "Result := gamma;"
      , "end;"
      , "except"
      , "on E: Exception do"
      , "chalk();"
      , "end;"
      ]
  , testCase "Complex try/with/case - except on..."
  $ (Right
      (Try
        [ With
            [(v "foo")]
            (Case
              (v "foo")
              [ CaseBranch [v "one"] (ExpressionValue (v "alpha"))
              , CaseBranch [v "two"] (ExpressionValue (v "beta"))
              ]
              (Just (Else (v "Result" := v "gamma")))
            )
        ]
        (Left [ExceptOn Nothing [ExpressionValue (v "chalk" :$ [])]])
      ) @=?
    )
  $ parse statement ""
  $ unpack
  $ intercalate
      "\n"
      [ "try"
      , "with foo do"
      , "case foo of"
      , "one: alpha;"
      , "two: beta;"
      , "else"
      , "Result := gamma;"
      , "end;"
      , "except"
      , "on E: Exception do"
      , "chalk();"
      , "end;"
      ]
  ]
