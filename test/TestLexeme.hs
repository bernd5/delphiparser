{-# LANGUAGE OverloadedStrings #-}
module TestLexeme
where

import Test.Tasty
import Test.Tasty.HUnit
import DelphiLexer
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char)
import DelphiAst (Lexeme(..))

lexemeTests :: TestTree
lexemeTests = testGroup "Lexeme Tests"
  [ testCase "char a"
  $ (Right (Lexeme "" 'a') @=?)
  $ parse (lexeme $ char 'a') "" "a"
  ]
