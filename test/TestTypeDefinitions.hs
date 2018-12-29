{-# LANGUAGE OverloadedStrings #-}

module TestTypeDefinitions ( typeDefinitionTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (typeAttribute', typeDefinition)
import Text.Megaparsec (parse)
import Data.Text(unpack, intercalate)

import Data.Maybe (Maybe(Just))

typeDefinitionTests :: TestTree
typeDefinitionTests = testGroup
  "Delphi Type Definition Tests"
  [ testGroup
      "TypeAttribute Tests"
      [ testCase "Type Attribute of Class"
      $ (Right
          (TypeAttribute [V "foo" :$ [S "hello"]]
                         (TypeAlias (Type "foo") (Type "bar"))
          ) @=?
        )
      $ parse typeAttribute' "" "[foo('hello')]\nfoo = bar;"
      , testCase "Ensure empty class parses"
      $ (Right (Class (Type "TFreeTheValue") [Type "TInterfacedObject"] []) @=?
        )
      $ parse typeDefinition
              ""
              "TFreeTheValue = class(TInterfacedObject) end;"
      , testCase "Ensure empty record parses"
      $ (Right (Record (Type "TFreeTheValue") []) @=?)
      $ parse typeDefinition "" "TFreeTheValue = record end;"
      , testCase "Ensure Forward Class parses"
      $ (Right (TypeAlias (Type "foo") (Type "class")) @=?)
      $ parse typeDefinition "" "foo = class;"
      , testCase "Ensure type function alias parses"
      $ (Right
          (TypeDef
            (Type "bar")
            (SimpleFunction [Arg ConstArg "foo" (Just $ Type "bar") Nothing]
                            (Type "string")
            )
          ) @=?
        )
      $ parse typeDefinition "" "bar = function(const foo:bar):string;"
      , testCase "Ensure 'class of' works"
      $ (Right (TypeDef (Type "foo") (ClassOf (Type "bar"))) @=?)
      $ parse typeDefinition "" "foo = class of bar;"
      , testCase "Ensure 'class helper for' works"
      $ (Right (TypeDef (Type "foo") (ClassHelper (Type "bar") [])) @=?)
      $ parse typeDefinition "" "foo = class helper for bar end;"
      , testCase "Ensure a record with a case parses"
      $ (Right (Record (Type "TFoo") [DefaultAccessibility [Field "name" (Type "string"),Field "desc" (Type "string"),CaseField (V "kind") [([V "kpFloat"],[]),([V "kpStr",V "kpPath"],[]),([V "kpInteger"],[Field "intvalue" (Type "longint")]),([V "kpDouble"],[Field "floatvalue" (Type "extended"),Field "abbr" (Type "Char")])] Nothing]]) @=? )
      $ parse typeDefinition ""
      $ unpack $ intercalate
          "\n"
          [ "TFoo=record"
          , "name,desc:string;"
          , "case kind: TBar of"
          , "kpFloat: ();"
          , "kpStr,kpPath: ();"
          , "kpInteger: (intvalue: longint);"
          , "kpDouble: (floatvalue: extended;"
          , "abbr: Char)"
          , "end"
          ]
      , testCase "Ensure another record with a case parses"
      $ (Right (Record (Type "TFoo") [DefaultAccessibility [Field "name" (Type "string"),Field "desc" (Type "string"),CaseField (V "kind") [([V "kpFloat"],[]),([V "kpStr",V "kpPath"],[]),([V "kpInteger"],[Field "intvalue" (Type "longint")]),([V "kpDouble"],[Field "floatvalue" (Type "extended"),Field "abbr" (Type "Char")])] Nothing]]) @=? )
      $ parse typeDefinition ""
      $ unpack $ intercalate
          "\n"
          [ "TFoo=record"
          , "name,desc:string;"
          , "case Boolean of"
          , "True: (Char: String;);"
          , "False: ();"
          , "end;"
          ]
      ]
  ]
