{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Text.IO (readFile)
import Prelude hiding (concatMap, readFile)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Char (isSpace)
import DelphiAst
import DelphiLexer
import DelphiParser
import DelphiWriter
import Text.Megaparsec (parse)

import Data.Maybe (Maybe(Just))

import TestArrays (arrayTests)
import TestDelphiCase (caseTests)
import TestDelphiTry (delphiTryTests)
import TestLoops (loopTests)
import TestProperties (propertiesTests)
import TestTypeArguments (typeArgumentTests)
import TestDelphiWriter (writerTests)
import TestTypeDefinitions (typeDefinitionTests)
import TestProcedureImplementation (procedureImplementationTest)
import TestDelphiFunctions (functionTests)
import TestLiterals (literalsTests)
import TestIfThen (ifThenTests)

newtype ParserTestsData = ParserTestsData
  { sharedpointer :: Text
  }

main :: IO ()
main = do
  sp <- readFile "deps/sharedpointer.pas"
  let p = ParserTestsData {sharedpointer = sp}
  defaultMain $ testGroup
    "Tests"
    [ unitTests p
    , arrayTests
    , loopTests
    , propertiesTests
    , typeArgumentTests
    , delphiTryTests
    , caseTests
    , writerTests
    , typeDefinitionTests
    , procedureImplementationTest
    , functionTests
    , literalsTests
    , ifThenTests
    ]

stripWhitespace :: Text -> Text
stripWhitespace = concatMap f
 where
  f :: Char -> Text
  f x = if isSpace x then "" else pack [x]

roundtrip :: ShowDelphi a => Text -> Parser a -> IO ()
roundtrip expected p = do
  let r = parse p "" $ unpack expected
  either (\_ -> assertFailure "Parse Error") actualCase r
 where
  actualCase actual = assertEqual "Delphi Generation must match source"
                                  (stripWhitespace expected)
                                  (stripWhitespace (showDelphi actual))

unitTests :: ParserTestsData -> TestTree
unitTests p = testGroup
  "Delphi Parser Tests"
  [ testCase "Ensure that show == read (if we ignore spaces)"
    $ roundtrip (sharedpointer p) dUnitP
  , testCase "Ensure that operations showDelphi are correct"
    $ roundtrip "23 + 32" dValueExpression
  , testCase
      "Ensure that function call value expression's showDelphi without a semicolon"
    $ roundtrip "foo(23)" dValueExpression
  , testCase
      "Ensure that function call^'s expression's showDelphi without a semicolon"
    $ roundtrip "(foo(23)^)" dValueExpression
  , testCase "Ensure that IndexCall showDelphi are correct"
    $ roundtrip "foo[(foo(32) + 32)]" expression'
  , testCase "Ensure that 'a < b > c' parses"
  $ (Right ((V "a" :< V "b") :> V "c") @=?)
  $ parse (parens "(" ")" expression') "" "(a < b > c)"
  , testCase "Ensure that 'a < b and b > c' parses"
  $ (Right (((V "a" :< V "b") :& V "b") :> V "c") @=?)
  $ parse (parens "(" ")" expression') "" "(a < b and b  > c)"
  , testCase "Ensure uses, with dots, parses"
  $ (Right [["one"], ["two", "alpha"], ["three"]] @=?)
  $ parse uses "" "uses one, two.alpha, three;"
  , testCase "Ensure usings parses"
  $ (Right [["one"], ["two"], ["three"]] @=?)
  $ parse uses "" "uses one, two, three;"
  , testCase "Underscores are valid as part of an identifier"
  $ (Right "foo_bar" @=?)
  $ parse identifier "" "foo_bar"
  , testCase "Ensure delphi skeleton parses"
  $ (Right
      (Unit "TestUnit"
            (Interface (Uses []) [TypeDefinitions []])
            (Implementation (Uses []) [])
            Initialization
            Finalization
      ) @=?
    )
  $ parse
      dUnitP
      "testUnit.pas"
      "unit TestUnit; interface type implementation initialization finalization end."
  , testCase "Ensure delphi skeleton beginning with a BOM parses"
  $ (Right
      (Unit "TestUnit"
            (Interface (Uses []) [TypeDefinitions []])
            (Implementation (Uses []) [])
            Initialization
            Finalization
      ) @=?
    )
  $ parse
      dUnitP
      "testUnit.pas"
      "\xFEFFunit TestUnit; interface type implementation initialization finalization end."
  , testCase
    "Ensure a Generic Member of a Generic Class with a Generic Result parses"
  $ (Right
      (MemberFunctionImpl (GenericInstance "TShared" [Type "T"])
                          (GenericInstance "Cast" [Type "TT"])
                          []
                          (GenericInstance "TShared" [Type "TT"])
                          []
                          []
                          (Begin [])
      ) @=?
    )
  $ parse dFunctionImplementationP ""
  $ unpack
  $ intercalate
      "\n"
      ["function TShared<T>.Cast<TT>: TShared<TT>;", "begin", "end;"]
  , testCase "Ensure comments at the start still parse"
  $ (Right
      (Unit "TestUnit"
            (Interface (Uses []) [TypeDefinitions []])
            (Implementation (Uses []) [])
            Initialization
            Finalization
      ) @=?
    )
  $ parse
      dUnitP
      "testUnit.pas"
      "{--\n This file starts with comments\n--}\n// And another comment\n\n\nunit TestUnit; interface type implementation initialization finalization end."
  , testCase "Ensure a class function declaration parses"
  $ (Right
      (Function (Type "foo")
                [Arg NormalArg "bar" (Just $ Type "TBar") Nothing]
                (Type "TBar")
                [Static]
      ) @=?
    )
  $ parse dFieldDefinitionP "" "class function foo(bar: TBar): TBar;"
  , testCase "Ensure a class function declaration parses - using dFunctionP"
  $ (Right
      (Function (Type "foo")
                [Arg NormalArg "bar" (Just $ Type "TBar") Nothing]
                (Type "TBar")
                [Static]
      ) @=?
    )
  $ parse dFunctionP "" "class function foo(bar: TBar): TBar;"
  , testCase "Ensure a simple function call parses using statement"
  $ (Right (ExpressionValue (V "SetLength" :$ [V "FList", V "FList"])) @=?)
  $ parse statement "" "SetLength(FList, FList);"
  , testCase "Ensure a nested function call parses"
  $ (Right
      (ExpressionValue (V "SetLength" :$ [V "FList", V "Length" :$ [V "FList"]])
      ) @=?
    )
  $ parse statement "" "SetLength(FList, Length(FList));"
  , testCase "Ensure a slightly complex nested function call parses"
  $ (Right (V "SetLength" :$ [V "FList", (V "Length" :$ [V "FList"]) :+ I 1]) @=?
    )
  $ parse expression' "" "SetLength(FList, Length(FList)+1);"
  , testCase "Ensure a(b)+c parses"
  $ (Right (V "a" :$ [V "b"] :+ V "c") @=?)
  $ parse expression' "" "a(b)+c"
  , testCase "Ensure that you can assign to the result of a cast"
  $ (Right
      (P [V "FFreeTheValue" `As` V "TFreeTheValue"] :. V "FObjectToFree" := Nil) @=?
    )
  $ parse statement "" "(FFreeTheValue as TFreeTheValue).FObjectToFree := nil;"
  , testCase "Ensure a value involving a generic type member function parses"
  $ (Right (((V "TShared" :<<>> [Type "TT"]) :. V "Create") :$ [Nil]) @=?)
  $ parse expression' "" "TShared<TT>.Create(nil)"
  , testCase "Ensure a parens'ed value parses" $ (Right (P [Nil]) @=?) $ parse
    expression'
    ""
    "(nil)"
  , testCase "Ensure a trivial reference value works"
  $ (Right (V "one") @=?)
  $ parse expression' "" "one"
  , testCase "Ensure an <> comparison works"
  $ (Right (V "one" :<> V "two") @=?)
  $ parse expression' "" "one <> two"
  , testCase "Ensure an and comparison works"
  $ (Right (V "one" :& V "two") @=?)
  $ parse expression' "" "one and two"
  , testCase "Ensure an as cast works"
  $ (Right (V "one" `As` V "two") @=?)
  $ parse expression' "" "one as two"
  , testCase "Ensure a < works" $ (Right (V "one" :< V "two") @=?) $ parse
    expression'
    ""
    "one<two"
  , testCase "Ensure dot operator works"
  $ (Right (V "one" :. V "two") @=?)
  $ parse expression' "" "one.two"
  , testCase "Ensure a simple function call works"
  $ (Right (V "one" :$ []) @=?)
  $ parse expression' "" "one()"
  , testCase "Ensure a simple function call with one arg works"
  $ (Right (V "one" :$ [V "two"]) @=?)
  $ parse expression' "" "one(two)"
  , testCase "Ensure a simple function call with two args works"
  $ (Right (V "one" :$ [V "two", V "three"]) @=?)
  $ parse expression' "" "one(two, three)"
  , testCase "Ensure that the empty statement works"
  $ (Right EmptyExpression @=?)
  $ parse statement "" ";"
  , testCase "Ensure assign to an index property parses"
  $ (Right (V "foo" :!! [I 32] := V "blah") @=?)
  $ parse statement "" "foo[32] := blah;"
  , testCase "Ensure reading an index property parses"
  $ (Right (ExpressionValue (V "foo" :!! [I 32])) @=?)
  $ parse statement "" "foo[32];"
  , testCase "Ensure reading an index property parses"
  $ (Right (V "foo" :!! [I 32]) @=?)
  $ parse expression' "" "foo[32];"
  , testCase "Ensure that foo.bar.baz.fuux is left associative"
  $ (Right (V "foo" :. V "bar" :. V "baz" :. V "fuux") @=?)
  $ parse expression' "" "foo.bar.baz.fuux"
  , testCase "Ensure a.b(c) syntax parses"
  $ (Right (V "a" :. V "b" :$ [V "c"]) @=?)
  $ parse expression' "" "a.b(c)"
  , testCase "Ensure a.b[c] syntax parses"
  $ (Right (V "a" :. V "b" :!! [V "c"]) @=?)
  $ parse expression' "" "a.b[c]"
  , testCase "Ensure index property parses"
  $ (Right (V "foo" :!! [I 32]) @=?)
  $ parse expression' "" "foo[32]"
  , testCase "Ensure simple type alias parses"
  $ (Right (TypeAlias (Type "foo") (Type "bar")) @=?)
  $ parse (typeAlias (Type "foo")) "" "bar"
  , testCase "Ensure hex integers parse" $ (Right (I 0x42) @=?) $ parse
    expression'
    ""
    "$42"
  , testCase "Ensure that arrays are a valid type"
  $ (Right (StaticArray (IndexOf [V "foo"]) (Type "bar")) @=?)
  $ parse typeName "" "array [foo] of bar"
  , testCase "Ensure that const expression's involving arrays parse"
  $ (Right
      (ConstDefinitions
        [ ConstDefinition
            "foo"
            (Just $ StaticArray (IndexOf [V "bar"]) (Type "baz"))
            (P [V "one", V "two", V "three"])
        ]
      ) @=?
    )
  $ parse constExpressions
          ""
          "const foo: array [bar] of baz = (one, two, three);"
  , testCase "Ensure foo.bar parses" $ (Right (V "foo" :. V "bar") @=?) $ parse
    expression'
    ""
    "foo.bar"
  , testCase "Ensure a = foo.bar parses"
  $ (Right (V "a" :== (V "foo" :. V "bar")) @=?)
  $ parse expression' "" "a = foo.bar"
  , testCase "Ensure if a = foo.bar then... parses"
  $ (Right
      (If (V "a" :== (V "foo" :. V "bar"))
          (Then (ExpressionValue (V "Nil")))
          (Else EmptyExpression)
      ) @=?
    )
  $ parse dIfExpression "" "if a = foo.bar then Nil;"
  , testCase "Ensure dereference parses"
  $ (Right (Dereference (V "foo")) @=?)
  $ parse expression' "" "^foo"
  , testCase "Ensure address-of parses"
  $ (Right (AddressOf (V "foo")) @=?)
  $ parse expression' "" "@foo"
  , testCase "Ensure foo <= bar parses"
  $ (Right (V "foo" :<= V "bar") @=?)
  $ parse expression' "" "foo <= bar"
  , testCase "Ensure that function call with indexed args parse"
  $ (Right (V "foo" := (V "bar" :$ [V "baz", DFalse, V "fuux" :!! [V "I"]])) @=?
    )
  $ parse statement "" "foo := bar(baz, false, fuux[I]);"
  , testCase "Ensure that function call with indexed args parse"
  $ (Right (V "bar" :$ [V "baz", DFalse, V "fuux" :!! [V "I"]]) @=?)
  $ parse expression' "" "bar(baz, false, fuux[I])"
  , testCase "Ensure var works"
  $ (Right
      (VarDefinitions
        [ VarDefinition "foo" (Type "bar")  Nothing
        , VarDefinition "baz" (Type "fuux") Nothing
        ]
      ) @=?
    )
  $ parse varExpressions "" "var foo: bar; baz: fuux;"
  , testCase "Ensure var works in a function"
  $ (Right
      (FunctionImpl
        (Type "foo")
        []
        (Type "bar")
        []
        [ AdditionalInterface
            (VarDefinitions
              [ VarDefinition "foo" (Type "bar")  Nothing
              , VarDefinition "baz" (Type "fuux") Nothing
              ]
            )
        ]
        (Begin [])
      ) @=?
    )
  $ parse functionImpl
          ""
          "function foo: bar; var foo: bar; baz: fuux; begin end;"
  , testCase "Ensure simple set parses"
  $ (Right (SetDefinition (Type "foo") (Type "bar")) @=?)
  $ parse (setDefinition (Type "foo")) "" "set of bar"
  , testCase "Ensure complete set example parses"
  $ (Right
      (Interface (Uses [])
                 [TypeDefinitions [SetDefinition (Type "foo") (Type "bar")]]
      ) @=?
    )
  $ parse dUnitInterfaceP "" "interface type foo = set of bar;"
  , testCase "Ensure a Generic Procedure of a Generic Class parses"
  $ (Right
      (MemberProcedureImpl (GenericInstance "TShared" [Type "T"])
                           (GenericInstance "Cast" [Type "TT"])
                           []
                           []
                           []
                           (Begin [])
      ) @=?
    )
  $ parse dProcedureImplementationP ""
  $ unpack
  $ intercalate "\n" ["procedure TShared<T>.Cast<TT>;", "begin", "end;"]
  ]
