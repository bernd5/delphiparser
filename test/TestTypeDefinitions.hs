{-# LANGUAGE OverloadedStrings #-}

module TestTypeDefinitions
  ( typeDefinitionTests
  )
where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.HUnit               ( testCase
                                                , (@=?)
                                                )

import           DelphiAst
import           DelphiParser                   ( typeAttribute'
                                                , typeDefinition
                                                , classType
                                                , dRecordDefinitionP
                                                )
import           Text.Megaparsec                ( parse )
import           Data.Text                      ( intercalate
                                                )

import           Data.Maybe                     ( Maybe(Just) )

typ a = Type $ Lexeme Empty a
arg a b c d = Arg a (Lexeme Empty b) c d
v a = V $ Lexeme Empty a
s a = S $ Lexeme Empty a
i a = I $ Lexeme Empty a
field a b = Field (Lexeme Empty a) b

typeDefinitionTests :: TestTree
typeDefinitionTests = testGroup
  "Delphi type Definition Tests"
  [ testGroup
      "TypeAttribute Tests"
      [ testCase "typ Attribute of Class"
      $ (Right
          (TypeAttribute [v "foo" :$ [s "hello"]]
                         (TypeAlias (typ "foo") (typ "bar"))
          ) @=?
        )
      $ parse typeAttribute' "" "[foo('hello')]\nfoo = bar;"
      , testCase "Ensure empty class parses"
      $ (Right
          (Class (typ "TFreeTheValue")
                 [typ "TInterfacedObject", typ "TFoo"]
                 []
          ) @=?
        )
      $ parse typeDefinition
              ""
              "TFreeTheValue = class(TInterfacedObject,TFoo){blah}end;"
      , testCase "Ensure empty record parses"
      $ (Right (Record (typ "TFreeTheValue") []) @=?)
      $ parse typeDefinition "" "TFreeTheValue = record end;"
      , testCase "Ensure Forward Class parses"
      $ (Right (ForwardClass (typ "foo")) @=?)
      $ parse typeDefinition "" "foo = class;"
      , testCase "Ensure type function alias parses"
      $ (Right
          (TypeDef
            (typ "bar")
            (SimpleFunction [arg ConstArg "foo" (Just $ typ "bar") Nothing]
                            (typ "string")
            )
          ) @=?
        )
      $ parse typeDefinition "" "bar = function(const foo:bar):string;"
      , testCase "Ensure 'class of' works"
      $ (Right (TypeDef (typ "foo") (ClassOf (typ "bar"))) @=?)
      $ parse typeDefinition "" "foo = class of bar;"
      , testCase "Ensure 'class helper for' works"
      $ (Right (TypeDef (typ "foo") (ClassHelper (typ "bar") [])) @=?)
      $ parse typeDefinition "" "foo = class helper for bar end;"
      , testCase "Ensure a record with a case parses"
      $ (Right
          (Record
            (typ "TFoo")
            [ DefaultAccessibility
                [ field "name" (typ "string")
                , field "desc" (typ "string")
                , CaseField
                  (v "kind")
                  [ ([v "kpFloat"]          , [])
                  , ([v "kpStr", v "kpPath"], [])
                  , ([v "kpInteger"], [field "intvalue" (typ "longint")])
                  , ( [v "kpDouble"]
                    , [ field "floatvalue" (typ "extended")
                      , field "abbr"       (typ "Char")
                      ]
                    )
                  , ([i 3] , [field "foovalue" (typ "Foo")])
                  ]
                  Nothing
                ]
            ]
          ) @=?
        )
      $ parse typeDefinition ""
      $ intercalate
          "\n"
          [ "TFoo=record"
          , "name,desc:string;"
          , "case kind: TBar of"
          , "kpFloat: ();"
          , "kpStr,kpPath: ();"
          , "kpInteger: (intvalue: longint);"
          , "kpDouble: (floatvalue: extended;"
          , "abbr: Char);"
          , "3: (foovalue: Foo)"
          , "end"
          ]
      , testCase "Ensure another record with a case parses"
      $ (Right
          (Record
            (typ "TFoo")
            [ DefaultAccessibility
                [ field "name" (typ "string")
                , field "desc" (typ "string")
                , CaseField
                  (v "Boolean")
                  [ ( [DTrue]
                    , [field "Char" (typ "String")]
                    )
                  , ([DFalse], [])
                  ]
                  Nothing
                ]
            ]
          ) @=?
        )
      $ parse typeDefinition ""
      $ intercalate
          "\n"
          [ "TFoo=record"
          , "name,desc:string;"
          , "case Boolean of"
          , "True: (Char: String;);"
          , "False: ();"
          , "end;"
          ]
      , testCase "Class with comments..."
      $ (Right
          (Class (typ "TFoo")
                 [typ "TObject", typ "IFoo"]
                 [Public []]
          ) @=?
        )
      $ parse (classType (typ "TFoo")) ""
      $ intercalate
          "\n"
          ["(TObject,IFoo){a}", "public{}", " {b}  ", "{h} end; {i}{j}"]
      , testCase "Class with comments..."
      $ (Right
          (Class
            (typ "TFoo")
            [typ "TObject"]
            [ Public
                [ Field (Lexeme (Comment "c") "name") (Type (Lexeme (Comment "f") "string"))
                , Field (Lexeme (Comment "e") "desc") (Type (Lexeme (Comment "f") "string"))
                ]
            ]
          ) @=?
        )
      $ parse (classType (typ "TFoo")) ""
      $ intercalate
          "\n"
          [ "(TObject){a}"
          , "public{}"
          , " {b}name{c},{d}desc{e}:{e}string{f} ; {g}"
          , "{h} end; {i}{j}"
          ]
      , testCase "RecordDefinition with comments..."
      $ (Right (Public [Field (Lexeme (Comment "") "name") (typ "string")]) @=?)
      $ parse (dRecordDefinitionP) ""
      $ intercalate "\n" ["public", "  { blah blah }  ", " {}name{}: string;"]
      , testCase "RecordDefinition without comments..."
      $ (Right (Public [field "name" (typ "string")]) @=?)
      $ parse (dRecordDefinitionP) ""
      $ intercalate "\n" ["public", " name : string;"]
      ]
  ]
