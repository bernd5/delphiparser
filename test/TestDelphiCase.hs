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
            (V $ Lexeme "" "foo")
            [ CaseBranch [V $ Lexeme "" "bar"] (ExpressionValue (V $ Lexeme "" "baz"))
            , CaseBranch [V $ Lexeme "" "fuux"] (ExpressionValue (V $ Lexeme "" "bar"))
            ]
            (Just (Else (ExpressionValue (V $ Lexeme "" "whatevs"))))) @=?) $
      parse delphiCase' "" "case foo of bar : baz; fuux: bar; else whatevs; end"
    ]
