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
import Text.Megaparsec (parse)

import Data.Maybe (Maybe(Just))

import TestLexeme (lexemeTests)
import TestTypeExpressions(typeExpressionTests)
import TestComments (commentTests)
import TestArrays (arrayTests)
import TestDelphiCase (caseTests)
import TestDelphiTry (delphiTryTests)
import TestTypeName (typeNameTests)
import TestLoops (loopTests)
import TestProperties (propertiesTests)
import TestTypeArguments (typeArgumentTests)
import TestTypeDefinitions (typeDefinitionTests)
import TestProcedureImplementation (procedureImplementationTest)
import TestDelphiFunctions (functionTests)
import TestLiterals (literalsTests)
import TestIfThen (ifThenTests)
import TestExpression(expressionTests)
import TestConst (constTests)

import TestSupport

newtype ParserTestsData = ParserTestsData
  { sharedpointer :: Text
  }

usesC (x : xs) = (Prelude.map (Lexeme []) x) : usesC xs
usesC _        = []
redirectedFunction a b = RedirectedFunction (Lexeme [] a) (Lexeme [] b)

main :: IO ()
main = do
  sp <- readFile "deps/sharedpointer.pas"
  let p = ParserTestsData {sharedpointer = sp}
  defaultMain $ testGroup
    "Tests"
    [ typeExpressionTests
    , lexemeTests
    , commentTests
    , unitTests p
    , arrayTests
    , loopTests
    , propertiesTests
    , typeArgumentTests
    , delphiTryTests
    , expressionTests
    , caseTests
    , typeDefinitionTests
    , procedureImplementationTest
    , functionTests
    , literalsTests
    , ifThenTests
    , typeNameTests
    , constTests
    ]

stripWhitespace :: Text -> Text
stripWhitespace = concatMap f
 where
  f :: Char -> Text
  f x = if isSpace x then "" else pack [x]

unitTests :: ParserTestsData -> TestTree
unitTests p = testGroup
  "Delphi Parser Tests"
  [ testCase "Ensure that 'a < b > c' parses"
  $ (Right ((v "a" :< v "b") :> v "c") @=?)
  $ parse (parens "(" ")" expression') "" "(a < b > c)"
  , testCase "Ensure that 'a < b and b > c' parses"
  $ (Right (((v "a" :< v "b") :& v "b") :> v "c") @=?)
  $ parse (parens "(" ")" expression') "" "(a < b and b  > c)"
  , testCase "Ensure uses, with dots, parses"
  $ (Right (usesC [["one"], ["two", "alpha"], ["three"]]) @=?)
  $ parse uses "" "uses one, two.alpha, three;"
  , testCase "Ensure usings parses"
  $ (Right (usesC [["one"], ["two"], ["three"]]) @=?)
  $ parse uses "" "uses one, two, three;"
  , testCase "Underscores are valid as part of an identifier"
  $ (Right (Lexeme [] "foo_bar") @=?)
  $ parse identifier "" "foo_bar"
  , testCase "Ensure delphi skeleton parses"
  $ (Right
      (Unit []
            (Lexeme [] "TestUnit")
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
      (MemberFunctionImpl (genericInstance "TShared" [typ "T"])
                          (genericInstance "Cast" [typ "TT"])
                          []
                          (genericInstance "TShared" [typ "TT"])
                          []
                          []
                          (Begin [])
      ) @=?
    )
  $ parse dFunctionImplementationP ""
  $ intercalate
      "\n"
      ["function TShared<T>.Cast<TT>: TShared<TT>;", "begin", "end;"]
  , testCase "Ensure comments at the start still parse"
  $ (Right
      (Unit [c "--\n This file starts with comments\n--", c " And another comment"]
            (Lexeme [] "TestUnit")
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
      [ Function (typ "foo")
                 [arg NormalArg "bar" (Just $ typ "TBar") Nothing]
                 (typ "TBar")
                 [Static]
      ] @=?
    )
  $ parse dFieldDefinitionP "" "class function foo(bar: TBar): TBar;"
  , testCase "Ensure a class var declaration parses"
  $ (Right [ClassVar (typ "foo") (typ "TBar")] @=?)
  $ parse dFieldDefinitionP "" "class var foo: TBar;"
  , testCase "Ensure a class var declaration that has a generic type parses"
  $ (Right
      [ClassVar (typ "foo") (genericInstance "TBar" [typ "TFoo", typ "TBaz"])] @=?
    )
  $ parse dFieldDefinitionP "" "class var foo: TBar<TFoo, TBaz>;"
  , testCase
    "Ensure a class function redirection parses - using dFieldDefinitionP"
  $ (Right [redirectedFunction "foo.bar" "baz"] @=?)
  $ parse dFieldDefinitionP "" "class function foo.bar= baz;"
  , testCase
    "Ensure a class function declaration parses - using dFieldDefinitionP"
  $ (Right
      [ Function (typ "foo")
                 [arg NormalArg "bar" (Just $ typ "TBar") Nothing]
                 (typ "TBar")
                 [Static]
      ] @=?
    )
  $ parse dFieldDefinitionP "" "class function foo(bar: TBar): TBar;"
  , testCase "Ensure a simple function call parses using statement"
  $ (Right (ExpressionValue (v "SetLength" :$ [v "FList", v "FList"])) @=?)
  $ parse statement "" "SetLength(FList, FList);"
  , testCase "Ensure a nested function call parses"
  $ (Right
      (ExpressionValue (v "SetLength" :$ [v "FList", v "Length" :$ [v "FList"]])
      ) @=?
    )
  $ parse statement "" "SetLength(FList, Length(FList));"
  , testCase "Ensure a slightly complex nested function call parses"
  $ (Right (v "SetLength" :$ [v "FList", (v "Length" :$ [v "FList"]) :+ i 1]) @=?
    )
  $ parse expression' "" "SetLength(FList, Length(FList)+1);"
  , testCase "Ensure a(b)+c parses"
  $ (Right (v "a" :$ [v "b"] :+ v "c") @=?)
  $ parse expression' "" "a(b)+c"
  , testCase "Ensure that you can assign to the result of a cast"
  $ (Right (ExpressionValue ((P [As (V (Lexeme [] "FFreeTheValue")) (V (Lexeme [] "TFreeTheValue"))] :. V (Lexeme [] "FObjectToFree")) :=. Nil)) @=? )
  $ parse statement "" "(FFreeTheValue as TFreeTheValue).FObjectToFree := nil;"
  , testCase "Ensure a value involving a generic type member function parses"
  $ (Right (((v "TShared" :<<>> [typ "TT"]) :. v "Create") :$ [Nil]) @=?)
  $ parse expression' "" "TShared<TT>.Create(nil)"
  , testCase "Ensure a parens'ed value parses" $ (Right (P [Nil]) @=?) $ parse
    expression'
    ""
    "(nil)"
  , testCase "Ensure a trivial reference value works"
  $ (Right (v "one") @=?)
  $ parse expression' "" "one"
  , testCase "Ensure an <> comparison works"
  $ (Right (v "one" :<> v "two") @=?)
  $ parse expression' "" "one <> two"
  , testCase "Ensure an and comparison works"
  $ (Right (v "one" :& v "two") @=?)
  $ parse expression' "" "one and two"
  , testCase "Ensure an as cast works"
  $ (Right (v "one" `As` v "two") @=?)
  $ parse expression' "" "one as two"
  , testCase "Ensure a < works" $ (Right (v "one" :< v "two") @=?) $ parse
    expression'
    ""
    "one<two"
  , testCase "Ensure dot operator works"
  $ (Right (v "one" :. v "two") @=?)
  $ parse expression' "" "one.two"
  , testCase "Ensure a simple function call works"
  $ (Right (v "one" :$ []) @=?)
  $ parse expression' "" "one()"
  , testCase "Ensure a simple function call with one arg works"
  $ (Right (v "one" :$ [v "two"]) @=?)
  $ parse expression' "" "one(two)"
  , testCase "Ensure a simple function call with two args works"
  $ (Right (v "one" :$ [v "two", v "three"]) @=?)
  $ parse expression' "" "one(two, three)"
  , testCase "Ensure that the empty statement works"
  $ (Right EmptyExpression @=?)
  $ parse statement "" ";"
  , testCase "Ensure assign to an index property parses"
  $ (Right (ExpressionValue ((V (Lexeme [] "foo") :!! [I (Lexeme [] 32)]) :=. V (Lexeme [] "blah"))) @=? )
  $ parse statement "" "foo[32] := blah;"
  , testCase "Ensure reading an index property parses"
  $ (Right (ExpressionValue (v "foo" :!! [i 32])) @=?)
  $ parse statement "" "foo[32];"
  , testCase "Ensure reading an index property parses"
  $ (Right (v "foo" :!! [i 32]) @=?)
  $ parse expression' "" "foo[32];"
  , testCase "Ensure that foo.bar.baz.fuux is left associative"
  $ (Right (v "foo" :. v "bar" :. v "baz" :. v "fuux") @=?)
  $ parse expression' "" "foo.bar.baz.fuux"
  , testCase "Ensure a.b(c) syntax parses"
  $ (Right (v "a" :. v "b" :$ [v "c"]) @=?)
  $ parse expression' "" "a.b(c)"
  , testCase "Ensure a.b[c] syntax parses"
  $ (Right (v "a" :. v "b" :!! [v "c"]) @=?)
  $ parse expression' "" "a.b[c]"
  , testCase "Ensure index property parses"
  $ (Right (v "foo" :!! [i 32]) @=?)
  $ parse expression' "" "foo[32]"
  , testCase "Ensure simple type alias parses"
  $ (Right (TypeAlias (typ "foo") (typ "bar")) @=?)
  $ parse (typeAlias (typ "foo")) "" "bar"
  , testCase "Ensure hex integers parse" $ (Right (i 0x42) @=?) $ parse
    expression'
    ""
    "$42"
  , testCase "Ensure that arrays are a valid type"
  $ (Right (StaticArray (IndexOf [v "foo"]) (typ "bar")) @=?)
  $ parse typeName "" "array [foo] of bar"
  , testCase "Ensure that const expression's involving arrays parse"
  $ (Right
      (ConstDefinitions
        [ ConstDefinition (Lexeme [] "foo")
                          (Just $ StaticArray (IndexOf [v "bar"]) (typ "baz"))
                          (P [v "one", v "two", v "three"])
        ]
      ) @=?
    )
  $ parse constExpressions
          ""
          "const foo: array [bar] of baz = (one, two, three);"
  , testCase "Ensure that const expression's involving arrays parse"
  $ (Right
      (ConstDefinitions
        [ ConstDefinition (Lexeme [] "foo")
                          (Just $ StaticArray (IndexOf [v "bar"]) (
                            StaticArray (IndexOf [i 30]) (typ "string")))
                          (P [s "one", s "two", s "three"])
        ]
      ) @=?
    )
  $ parse constExpressions
          ""
          "const foo: array [bar] of string[30] = ('one', 'two', 'three');"
  , testCase "Ensure foo.bar parses" $ (Right (v "foo" :. v "bar") @=?) $ parse
    expression'
    ""
    "foo.bar"
  , testCase "Ensure a = foo.bar parses"
  $ (Right (v "a" :== (v "foo" :. v "bar")) @=?)
  $ parse expression' "" "a = foo.bar"
  , testCase "Ensure if a = foo.bar then... parses"
  $ (Right
      (If (V (Lexeme [] "a") :== (V (Lexeme [] "foo") :. V (Lexeme [] "bar")))
          (Then (ExpressionValue Nil))
          (Else EmptyExpression)
      ) @=?
    )
  $ parse dIfExpression "" "if a = foo.bar then Nil;"
  , testCase "Ensure dereference parses"
  $ (Right (Dereference (v "foo")) @=?)
  $ parse expression' "" "^foo"
  , testCase "Ensure address-of parses"
  $ (Right (AddressOf (v "foo")) @=?)
  $ parse expression' "" "@foo"
  , testCase "Ensure foo <= bar parses"
  $ (Right (v "foo" :<= v "bar") @=?)
  $ parse expression' "" "foo <= bar"
  , testCase "Ensure that function call with indexed args parse"
  $ (Right (v "foo" := (v "bar" :$ [v "baz", DFalse, v "fuux" :!! [v "I"]])) @=?
    )
  $ parse statement "" "foo := bar(baz, false, fuux[I]);"
  , testCase "Ensure that function call with indexed args parse"
  $ (Right (v "bar" :$ [v "baz", DFalse, v "fuux" :!! [v "I"]]) @=?)
  $ parse expression' "" "bar(baz, false, fuux[I])"
  , testCase "Ensure var works"
  $ (Right
      (VarDefinitions
        [ varDefinition "foo" (typ "bar")  Nothing
        , varDefinition "baz" (typ "fuux") Nothing
        ]
      ) @=?
    )
  $ parse varExpressions "" "var foo: bar; baz: fuux;"
  , testCase "Ensure var works in a function"
  $ (Right
      (FunctionImpl
        (typ "foo")
        []
        (typ "bar")
        []
        [ AdditionalInterface
            (VarDefinitions
              [ varDefinition "foo" (typ "bar")  Nothing
              , varDefinition "baz" (typ "fuux") Nothing
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
  $ (Right (SetDefinition (typ "foo") (typ "bar")) @=?)
  $ parse (setDefinition (typ "foo")) "" "set of bar"
  , testCase "Ensure complete set example parses"
  $ (Right
      (Interface (Uses [])
                 [TypeDefinitions [SetDefinition (typ "foo") (typ "bar")]]
      ) @=?
    )
  $ parse dUnitInterfaceP "" "interface type foo = set of bar;"
  , testCase "Ensure a Generic Procedure of a Generic Class parses"
  $ (Right
      (MemberProcedureImpl (genericInstance "TShared" [typ "T"])
                           (genericInstance "Cast" [typ "TT"])
                           []
                           []
                           []
                           (Begin [])
      ) @=?
    )
  $ parse dProcedureImplementationP ""
  $ intercalate "\n" ["procedure TShared<T>.Cast<TT>;", "begin", "end;"]
  , testCase "Ensure a static constructor implementation parses"
  $ (Right
      (Implementation
        (Uses [])
        [ MemberConstructorImpl (typ "TShared")
                                (typ "Create")
                                []
                                []
                                []
                                (Begin [])
        ]
      ) @=?
    )
  $ parse dUnitImplementationP ""
  $ intercalate
      "\n"
      ["implementation class constructor TShared.Create;", "begin", "end;"]
  ]
