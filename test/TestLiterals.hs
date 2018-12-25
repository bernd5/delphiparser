{-# LANGUAGE OverloadedStrings #-}

module TestLiterals ( literalsTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (expression', singleVarExpression)
import DelphiWriter

import DelphiAst

literalsTests :: TestTree
literalsTests = testGroup
  "Delphi Literal Expressions"
  [ testGroup "Sets and Array Literals"
    $ [ testCase "[a, b, c]" 
      $ (Right (L [V "a",V "b",V "c"]) @=?) $ parse
          expression'
          ""
          "[a, b, c]"
      , testCase "[1, 2, 3]" 
      $ (Right (L [I 1, I 2, I 3]) @=?) $ parse
          expression'
          ""
          "[1, 2, 3]"
      ]
  , testGroup "var"
    $ [ testCase "Floating point: 42.56" $ (Right [VarDefinition "A" (Type "float") (Just (F 42.56))] @=?) $ parse
          singleVarExpression
          ""
          "A: float = 42.56;"
      , testCase "Integer: 42 -> string" $ 
          ("A: int=42" @=?)
          $ showDelphi (VarDefinition "A" (Type "int") (Just (I 42)))
      ]
  , testGroup "Number Literals"
    $ [ testCase "Floating point: 42.56" $ (Right (F 42.56) @=?) $ parse
          expression'
          ""
          "42.56"
      , testCase "Integer: 4256" $ (Right (I 4256) @=?) $ parse
          expression'
          ""
          "4256"
      ]
  , testGroup "Literal Array Expressions"
    [ testCase "[1..5]" $
      (Right (L [I 1 :.. I 5]) @=? ) $
      parse expression' "" "[1..5]"
    , testCase "['a'..'f']" $
      (Right (L [S "a" :.. S "f"]) @=? ) $
      parse expression' "" "['a'..'f']"
    , testCase "['a'..'f', 'w'..'z']" $
      (Right (L [S "a" :.. S "f", S "w" :.. S "z"]) @=? ) $
      parse expression' "" "['a'..'f', 'w'..'z']"
    , testCase "[a]" $
      (Right (L [V "a"]) @=? ) $
      parse expression' "" "[a]"
    , testCase "[a.b]" $
      (Right (L [V "a" :. V "b"]) @=? ) $
      parse expression' "" "[a.b]"
    ]
  ]
