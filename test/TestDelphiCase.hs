{-# LANGUAGE OverloadedStrings #-}

module TestDelphiCase
  ( caseTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import DelphiParser (delphiCase')
import Text.Megaparsec (parse)

import DelphiAst

v a = V (Lexeme Empty a)

caseTests :: TestTree
caseTests =
  testGroup
    "Delphi try/finally/except Tests"
    [ testCase "simple case" $
      (Right
         (Case
            (v "foo")
            [ CaseBranch [v "bar"] (ExpressionValue (v "baz"))
            , CaseBranch [v "fuux"] (ExpressionValue (v "bar"))
            ]
            (Just (Else (ExpressionValue (v "whatevs"))))) @=?) $
      parse delphiCase' "" "case foo of bar : baz; fuux: bar; else whatevs; end"
    ]
