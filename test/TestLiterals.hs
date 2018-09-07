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
    $ [ testCase "[a, b, c]" $ (Right (L [V "a", V "b", V "c"]) @=?) $ parse
          expression'
          ""
          "[a, b, c]"
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
  ]
