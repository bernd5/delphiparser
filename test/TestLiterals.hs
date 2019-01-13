{-# LANGUAGE OverloadedStrings #-}

module TestLiterals ( literalsTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (expression', singleVarExpression)
import DelphiWriter

import DelphiAst

v a = V $ Lexeme "" a
i a = I $ Lexeme "" a
s a = S $ Lexeme "" a
typ a = Type $ Lexeme "" a
varDefinition a b c = VarDefinition (Lexeme "" a) b c

literalsTests :: TestTree
literalsTests = testGroup
  "Delphi Literal Expressions"
  [ testGroup "Sets and Array Literals"
    $ [ testCase "[a, b, c]" 
      $ (Right (L [v "a",v "b",v"c"]) @=?) $ parse
          expression'
          ""
          "[a, b, c]"
      , testCase "[1, 2, 3]" 
      $ (Right (L [i 1, i 2, i 3]) @=?) $ parse
          expression'
          ""
          "[1, 2, 3]"
      , testCase "(a, b, c)" 
      $ (Right (P [v "a",v "b",v"c"]) @=?) $ parse
          expression'
          ""
          "(a, b, c)"
      , testCase "(1, 2, 3)" 
      $ (Right (P [i 1, i 2, i 3]) @=?) $ parse
          expression'
          ""
          "(1, 2, 3)"
      ]
  , testGroup "var"
    $ [ testCase "Floating point: 42.56" $ (Right [varDefinition "A" (typ "float") (Just (F 42.56))] @=?) $ parse
          singleVarExpression
          ""
          "A: float = 42.56;"
      , testCase "Integer: 42 -> string" $ 
          ("A: int=42" @=?)
          $ showDelphi (varDefinition "A" (typ "int") (Just (i 42)))
      ]
  , testGroup "Number Literals"
    $ [ testCase "Floating point: 42.56" $ (Right (F 42.56) @=?) $ parse
          expression'
          ""
          "42.56"
      , testCase "Integer: 4256" $ (Right (i 4256) @=?) $ parse
          expression'
          ""
          "4256"
      ]
  , testGroup "Literal Array Expressions"
    [ testCase "[1..5]" $
      (Right (L [i 1 :.. i 5]) @=? ) $
      parse expression' "" "[1..5]"
    , testCase "['a'..'f']" $
      (Right (L [s "a" :.. s "f"]) @=? ) $
      parse expression' "" "['a'..'f']"
    , testCase "['a'..'f', 'w'..'z']" $
      (Right (L [s "a" :.. s "f", s "w" :.. s "z"]) @=? ) $
      parse expression' "" "['a'..'f', 'w'..'z']"
    , testCase "[a]" $
      (Right (L [v "a"]) @=? ) $
      parse expression' "" "[a]"
    , testCase "[a.b]" $
      (Right (L [v "a" :. v "b"]) @=? ) $
      parse expression' "" "[a.b]"
    ]
  ]
