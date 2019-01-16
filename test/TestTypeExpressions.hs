{-# LANGUAGE OverloadedStrings #-}

module TestTypeExpressions ( typeExpressionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (typeAttribute', typeExpressions, newType)
import Text.Megaparsec (parse)
import Data.Text(unpack, intercalate)

import Data.Maybe (Maybe(Just))

typ a = Type $ Lexeme Empty a
arg a b c d = Arg a (Lexeme Empty b) c d
v a = V $ Lexeme Empty a
s a = S $ Lexeme Empty a
i a = I $ Lexeme Empty a
field a b = Field (Lexeme Empty a) b

typeExpressionTests :: TestTree
typeExpressionTests = testGroup
  "Delphi type Expression Tests"
  [ testCase "Type Expression"
  $ (Right (TypeDefinitions [ForwardClass (typ "foo"),ForwardClass (typ "bar")]) @=? )
  $ parse typeExpressions "" "type foo = class; bar=class;"
  , testCase "Uppercase Type Expression followed by comment"
  $ (Right (TypeDefinitions
      [ ForwardClass (typ "foo")
      , ForwardClass (typ "bar")
      ]) @=? )
  $ parse typeExpressions "" "TYPE foo = class; bar=class;\n {hey there}"
  , testCase "Type, all on it's own"
  $ (Right (TypeDefinitions []) @=? )
  $ parse typeExpressions "" "Type\n "
  , testCase "Type, followed by Var"
  $ (Right (TypeDefinitions []) @=? )
  $ parse typeExpressions "" "Type\n\n\nVar"
  , testCase "Type, followed by var"
  $ (Right (TypeDefinitions []) @=? )
  $ parse typeExpressions "" "Type\n\n\nvar"
  , testCase "type foo = type bar;"
  $ (Right (TypeDefinitions [TypeDef (typ "foo") (NewType (typ "bar"))]) @=? )
  $ parse typeExpressions "" "type foo = type bar;"
  , testCase "type foo = 1..3;"
  $ (Right (TypeDefinitions [TypeExpression (i 1 :.. i 3)]) @=? )
  $ parse typeExpressions "" "type foo = 1..3;"
  , testCase "type bar"
  $ (Right (TypeDef (typ "a") (NewType (typ "bar"))) @=?)
  $ parse (newType (typ "a"))  "" "type bar"
  ]
