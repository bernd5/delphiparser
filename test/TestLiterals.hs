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
      $ (Right (A (IndexOf [Type "a",Type "b",Type "c"])) @=?) $ parse
          expression'
          ""
          "[a, b, c]"
      , testCase "[1, 2, 3]" 
      $ (Right (A (IndexOfE [I 1, I 2, I 3])) @=?) $ parse
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
      (Right (A $ Range [(I 1, I 5)]) @=? ) $
      parse expression' "" "[1..5]"
    , testCase "['a'..'f']" $
      (Right (A $ Range [(S "a", S "f")]) @=? ) $
      parse expression' "" "['a'..'f']"
    , testCase "['a'..'f', 'w'..'z']" $
      (Right (A $ Range [(S "a", S "f"), (S "w", S "z")]) @=? ) $
      parse expression' "" "['a'..'f', 'w'..'z']"
    , testCase "[a.b]" $
      (Right (A $ IndexOf [Type "a.b"]) @=? ) $
      parse expression' "" "[a.b"
    ]
  ]
