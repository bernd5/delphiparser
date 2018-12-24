{-# LANGUAGE OverloadedStrings #-}

module TestExpression (expressionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec (parse)
import DelphiAst
import DelphiParser (expression')

expressionTests :: TestTree
expressionTests = testGroup
  "Delphi Expressions and Precedence Tests"
  [ testGroup
    "Left Precedence"
    [ testCase "a + b + c" $ (Right ((V "a") :+ (V "b") :+ (V "c")) @=?) $ parse
      expression'
      ""
      "a + b + c"
    , testCase "a - b + c" $ (Right ((V "a") :- (V "b") :+ (V "c")) @=?) $ parse
      expression'
      ""
      "a - b + c"
    , testCase "a / b + c" $ (Right ((V "a") :/ (V "b") :+ (V "c")) @=?) $ parse
      expression'
      ""
      "a / b + c"
    , testCase "a * b + c" $ (Right ((V "a") :* (V "b") :+ (V "c")) @=?) $ parse
      expression'
      ""
      "a * b + c"
    ]
  , testGroup
    "Right Precedence"
    [ testCase "a + b / c"
    $ (Right ((V "a") :+ ((V "b") :/ (V "c"))) @=?)
    $ parse expression' "" "a + b / c"
    , testCase "a + b * c"
    $ (Right ((V "a") :+ ((V "b") :* (V "c"))) @=?)
    $ parse expression' "" "a + b * c"
    , testCase "a .. b * c"
    $ (Right ((V "a") :.. ((V "b") :* (V "c"))) @=?)
    $ parse expression' "" "a .. b * c"
    ]
  , testGroup
    "Middle precedence"
    [ testCase "a + b * c + d"
    $ (Right (V "a" :+ (V "b" :* V "c") :+ V "d") @=?)
    $ parse expression' "" "a + b * c + d"
    ]
  , testGroup
    "Prefixes, Postfixes and infixes"
    [ testCase "^a.b^.c"
    $ (Right (Dereference (Dereference (V "a") :. V "b") :. V "c") @=?)
    $ parse expression' "" "^a.b^.c"
    , testCase "a and b.c"
    $ (Right (V "a" :& (V "b" :. V "c") ) @=?)
    $ parse expression' "" "a and b.c"
    ]
  , testGroup
    "Dots..."
    [ testCase "a.b"
    $ (Right (V "a" :. V "b") @=?)
    $ parse expression' "" "a.b"
    , testCase "a.b.c"
    $ (Right (V "a" :. V "b" :. V "c") @=?)
    $ parse expression' "" "a.b.c"
    , testCase "a(b).c.d"
    $ (Right ((V "a" :$ [V "b"]) :. V "c" :. V "d") @=?)
    $ parse expression' "" "a(b).c.d"
    , testCase "(a(b)).c.d"
    $ (Right (P [(V "a" :$ [V "b"])] :. V "c" :. V "d") @=?)
    $ parse expression' "" "(a(b)).c.d"
    , testCase "a<b>.c.d"
    $ (Right ((V "a" :<<>> [Type "b"]) :. V "c" :. V "d") @=?)
    $ parse expression' "" "a<b>.c.d"
    , testCase "a.b(c).d"
    $ (Right ((V "a" :. V "b" :$ [V "c"]) :. V "d") @=?)
    $ parse expression' "" "a.b(c).d"
    ]
  ]
