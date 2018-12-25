{-# LANGUAGE OverloadedStrings #-}

module TestExpression (expressionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec (parse, try, many)
import DelphiAst
import DelphiLexer
import DelphiParser (expression', dBeginEndExpression, interfaceItems, typeName )
import DelphiExpressions

lambdaFunction' :: Parser ValueExpression
lambdaFunction' = lambdaFunction dBeginEndExpression interfaceItems typeName

lambdaArgs' :: Parser [Argument]
lambdaArgs' = lambdaArgs dBeginEndExpression interfaceItems typeName

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
    , testCase "-a * b + c" $ (Right ((Negate $ V "a") :* (V "b") :+ (V "c")) @=?) $ parse
      expression'
      ""
      "-a * b + c"
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
  , testGroup
    "Literals"
    [ testCase "[]"
    $ (Right (L []) @=?)
    $ parse expression' "" "[]"
    , testCase "[1]"
    $ (Right (L [I 1]) @=?)
    $ parse expression' "" "[1]"
    , testCase "[1, 2]"
    $ (Right (L [I 1, I 2]) @=?)
    $ parse expression' "" "[1, 2]"
    ]
  , testGroup
    "Lambda..."
    [ testCase "function():Boolean begin end"
    $ (Right (LambdaFunction [] (Type "Boolean") [] (Begin [])) @=?)
    $ parse expression' "" "function(): Boolean begin end"
    , testCase "function ():Boolean begin end"
    $ (Right (LambdaFunction [] (Type "Boolean") [] (Begin [])) @=?)
    $ parse lambdaFunction' "" "function(): Boolean begin end"
    , testCase "Lambda arguments: ()"
    $ (Right ([]) @=?)
    $ parse lambdaArgs' "" "()"
    , testCase ":Boolean;"
    $ (Right (Type "Boolean") @=?)
    $ parse (symbol ":" *> typeName <* semi) "" ": Boolean;"
    , testCase "Interface Items: <empty>"
    $ (Right [] @=?)
    $ parse (many $ try interfaceItems) "" "  begin end"
    , testCase "Statements: begin end"
    $ (Right (Begin []) @=?)
    $ parse (try dBeginEndExpression) "" "begin end"
    ]
  , testGroup
    "Strings..."
    [ testCase "'foo'"
    $ (Right (S "foo") @=?)
    $ parse expression' "" "'foo'"
    , testCase "'foo''bar'"
    $ (Right (S "foobar") @=?)
    $ parse expression' "" "'foo''bar'"
    , testCase "'foo'#42'bar'"
    $ (Right (S "foo*bar") @=?)
    $ parse expression' "" "'foo'#42'bar'"
    , testCase "#42#42"
    $ (Right (S "**") @=?)
    $ parse expression' "" "#42#42"
    ]
  ]

