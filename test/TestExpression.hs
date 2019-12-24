{-# LANGUAGE OverloadedStrings #-}

module TestExpression (expressionTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec (parse, try, many)
import DelphiAst
import DelphiLexer
import DelphiParser (expression', beginEndExpression, interfaceItems, typeName )
import TestSupport

expressionTests :: TestTree
expressionTests = testGroup
  "Delphi Expressions and Precedence Tests"
  [ testGroup
    "Left Precedence"
    [ testCase "a + b + c" $ (Right ((v "a") :+ (v "b") :+ (v "c")) @=?) $ parse
      expression'
      ""
      "a + b + c"
    , testCase "a - b + c" $ (Right ((v "a") :- (v "b") :+ (v "c")) @=?) $ parse
      expression'
      ""
      "a - b + c"
    , testCase "a / b + c" $ (Right ((v "a") :/ (v "b") :+ (v "c")) @=?) $ parse
      expression'
      ""
      "a / b + c"
    , testCase "a * b + c" $ (Right ((v "a") :* (v "b") :+ (v "c")) @=?) $ parse
      expression'
      ""
      "a * b + c"
    , testCase "-a * b + c" $ (Right ((Negate $ v "a") :* (v "b") :+ (v "c")) @=?) $ parse
      expression'
      ""
      "-a * b + c"
    ]
  , testGroup
    "Right Precedence"
    [ testCase "a + b / c"
    $ (Right ((v "a") :+ ((v "b") :/ (v "c"))) @=?)
    $ parse expression' "" "a + b / c"
    , testCase "a + b * c"
    $ (Right ((v "a") :+ ((v "b") :* (v "c"))) @=?)
    $ parse expression' "" "a + b * c"
    , testCase "a .. b * c"
    $ (Right ((v "a") :.. ((v "b") :* (v "c"))) @=?)
    $ parse expression' "" "a .. b * c"
    ]
  , testGroup
    "Middle precedence"
    [ testCase "a + b * c + d"
    $ (Right (v "a" :+ (v "b" :* v "c") :+ v "d") @=?)
    $ parse expression' "" "a + b * c + d"
    ]
  , testGroup
    "Prefixes, Postfixes and infixes"
    [ testCase "^a.b^.c"
    $ (Right (Dereference (Dereference (v "a") :. v "b") :. v "c") @=?)
    $ parse expression' "" "^a.b^.c"
    , testCase "a and b.c"
    $ (Right (v "a" :& (v "b" :. v "c") ) @=?)
    $ parse expression' "" "a and b.c"
    ]
  , testGroup
    "Dots..."
    [ testCase "a.b"
    $ (Right (v "a" :. v "b") @=?)
    $ parse expression' "" "a.b"
    , testCase "a.b.c"
    $ (Right (v "a" :. v "b" :. v "c") @=?)
    $ parse expression' "" "a.b.c"
    , testCase "a(b).c.d"
    $ (Right ((v "a" :$ [v "b"]) :. v "c" :. v "d") @=?)
    $ parse expression' "" "a(b).c.d"
    , testCase "(a(b)).c.d"
    $ (Right (P [(v "a" :$ [v "b"])] :. v "c" :. v "d") @=?)
    $ parse expression' "" "(a(b)).c.d"
    , testCase "a<b>.c.d"
    $ (Right ((v "a" :<<>> [typ "b"]) :. v "c" :. v "d") @=?)
    $ parse expression' "" "a<b>.c.d"
    , testCase "a.b(c).d"
    $ (Right ((v "a" :. v "b" :$ [v "c"]) :. v "d") @=?)
    $ parse expression' "" "a.b(c).d"
    ]
  , testGroup
    "Literals"
    [ testCase "[]"
    $ (Right (L []) @=?)
    $ parse expression' "" "[]"
    , testCase "[1]"
    $ (Right (L [i 1]) @=?)
    $ parse expression' "" "[1]"
    , testCase "[1, 2]"
    $ (Right (L [i 1, i 2]) @=?)
    $ parse expression' "" "[1, 2]"
    ]
  , testGroup
    "Lambda..."
    [ testCase "function():Boolean begin end"
    $ (Right (LambdaFunction [] (typ "Boolean") [] (Begin [])) @=?)
    $ parse expression' "" "function(): Boolean begin end"
    , testCase "function ():Boolean begin end"
    $ (Right (LambdaFunction [] (typ "Boolean") [] (Begin [])) @=?)
    $ parse lambdaFunction' "" "function(): Boolean begin end"
    , testCase "Lambda arguments: ()"
    $ (Right ([]) @=?)
    $ parse lambdaArgs' "" "()"
    , testCase ":Boolean;"
    $ (Right (typ "Boolean") @=?)
    $ parse (symbol ":" *> typeName <* semi) "" ": Boolean;"
    , testCase "Interface Items: <empty>"
    $ (Right [] @=?)
    $ parse (many $ try interfaceItems) "" "  begin end"
    , testCase "Statements: begin end"
    $ (Right (Begin []) @=?)
    $ parse (try beginEndExpression) "" "begin end"
    ]
  , testGroup
    "Strings..."
    [ testCase "'foo'"
    $ (Right (s "foo") @=?)
    $ parse expression' "" "'foo'"
    , testCase "'foo''bar'"
    $ (Right (s "foobar") @=?)
    $ parse expression' "" "'foo''bar'"
    , testCase "'foo'#42'bar'"
    $ (Right (s "foo*bar") @=?)
    $ parse expression' "" "'foo'#42'bar'"
    , testCase "#42#42"
    $ (Right (s "**") @=?)
    $ parse expression' "" "#42#42"
    ]
  , testGroup
    "Misc"
    [ testCase "3 : (blah: blah)"
    $ (Right (i 3) @=? )
    $ parse expression' "" "3 : (blah: blah)\n"
    ]
  ]

