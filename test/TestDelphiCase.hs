{-# LANGUAGE OverloadedStrings #-}

module TestDelphiCase
  ( caseTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import DelphiParser (delphiCase')
import Text.Megaparsec (parse)

import DelphiAst

caseTests :: TestTree
caseTests =
  testGroup
    "Delphi try/finally/except Tests"
    [ testCase "simple case" $
      (Right
         (Case
            (V "foo")
            [ CaseBranch [V "bar"] (ExpressionValue (V "baz"))
            , CaseBranch [V "fuux"] (ExpressionValue (V "bar"))
            ]
            (Just (Else (ExpressionValue (V "whatevs"))))) @=?) $
      parse delphiCase' "" "case foo of bar : baz; fuux: bar; else whatevs; end"
    ]
