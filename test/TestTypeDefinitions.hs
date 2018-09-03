{-# LANGUAGE OverloadedStrings #-}

module TestTypeDefinitions ( typeDefinitionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (typeAttribute')
import Text.Megaparsec (parse)

typeDefinitionTests :: TestTree
typeDefinitionTests = testGroup
  "Delphi Type Definition Tests"
  [ testGroup
    "TypeAttribute Tests"
    [ testCase "Type Attribute of Class"
    $ (Right (TypeAttribute [ V "foo" :$ [S "hello" ]] (TypeAlias (Type "foo") (Type "bar"))) @=? )
    $ parse typeAttribute' "" "[foo('hello')]\nfoo = bar;"
    ]
  ]
