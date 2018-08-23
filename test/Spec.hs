{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Text.IO (readFile)
import Prelude hiding (readFile)

import Test.Tasty
import Test.Tasty.HUnit

import DelphiAst
import DelphiLexer
import DelphiParser
import DelphiWriter
import Text.Megaparsec (parse)

data ParserTestsData = ParserTestsData
  { sharedpointer :: Text
  }

main :: IO ()
main = do
  sp <- readFile "deps/sharedpointer.pas"
  let p = ParserTestsData {sharedpointer = sp}
  defaultMain $ testGroup "Tests" [unitTests p]

roundtrip expected p = do
  let r = parse p "" $ unpack expected
  either
    (\_ -> assertFailure "Parse Error")
    (\actual ->
       assertEqual
         "Delphi Generation must match source"
         expected
         (showDelphi actual))
    r

unitTests p =
  testGroup
    "Delphi Parser Tests"
    [ testCase "Ensure that show == read" $ roundtrip (sharedpointer p) dUnitP
    , testCase "Ensure that operations showDelphi are correct" $
      roundtrip "( 23 + 32 )" dValueExpression
    , testCase
        "Ensure that function call value expressions showDelphi without a semicolon" $
      roundtrip "foo(23)" dValueExpression
    , testCase "Ensure that IndexCall showDelphi are correct" $
      roundtrip "foo[( foo(32) + 32 )]" dValueP
    , testCase "Ensure delphi skeleton parses" $
      (Right
         (Unit
            "TestUnit"
            (Interface [])
            (Implementation [])
            Initialization
            Finalization) @=?) $
      parse
        dUnitP
        "testUnit.pas"
        "unit TestUnit; interface type implementation initialization finalization end."
    , testCase
        "Ensure a Generic Member of a Generic Class with a Generic Result parses" $
      (Right
         (MemberFunctionImpl
            (GenericInstance "TShared" [Type "T"])
            (GenericInstance "Cast" [Type "TT"])
            []
            (GenericInstance "TShared" [Type "TT"])
            (Begin [])) @=?) $
      parse dFunctionImplementationP "" $
      unpack $
      intercalate
        "\n"
        ["function TShared<T>.Cast<TT>: TShared<TT>;", "begin", "end;"]
    , testCase "Ensure empty Record parses" $
      (Right [Class (Type "TFreeTheValue") [Type "TInterfacedObject"] []] @=?) $
      parse dTypeSpecListP "" "TFreeTheValue = class(TInterfacedObject) end;"
    , testCase "Ensure a Generic Procedure of a Generic Class parses" $
      (Right
         (MemberProcedureImpl
            (GenericInstance "TShared" [Type "T"])
            (GenericInstance "Cast" [Type "TT"])
            []
            (Begin [])) @=?) $
      parse dProcedureImplementationP "" $
      unpack $
      intercalate "\n" ["procedure TShared<T>.Cast<TT>;", "begin", "end;"]
    , testCase "Ensure a simple function call parses using dStatementP" $
      (Right
         (ExpressionValue
            (FunctionCall
               (SimpleReference "SetLength")
               [SimpleReference "FList", SimpleReference "FList"])) @=?) $
      parse dStatementP "" "SetLength(FList, FList);"
    , testCase "Ensure a nested function call parses" $
      (Right
         (ExpressionValue
            (FunctionCall
               (SimpleReference "SetLength")
               [ SimpleReference "FList"
               , FunctionCall
                   (SimpleReference "Length")
                   [SimpleReference "FList"]
               ])) @=?) $
      parse dStatementP "" "SetLength(FList, Length(FList));"
    , testCase "Ensure a slightly complex nested function call parses" $
      (Right
         (ExpressionValue
            (FunctionCall
               (SimpleReference "SetLength")
               [ SimpleReference "FList"
               , Operation
                   (FunctionCall
                      (SimpleReference "Length")
                      [SimpleReference "FList"])
                   "+"
                   (Integer 1)
               ])) @=?) $
      parse dStatementP "" "SetLength(FList, Length(FList)+1);"
    , testCase "Ensure that you can assign to the result of a cast" $
      (Right
         (Assign
            (MemberAccess
               (Operation
                  (SimpleReference "FFreeTheValue")
                  "as"
                  (SimpleReference "TFreeTheValue"))
               (SimpleReference "FObjectToFree"))
            Nil) @=?) $
      parse
        dStatementP
        ""
        "(FFreeTheValue as TFreeTheValue).FObjectToFree := nil;"
    , testCase "Ensure a value involving a generic type member function parses" $
      (Right
         (TypeMemberRef
            (GenericInstance "TShared" [Type "TT"])
            (Type "Create")
            [Nil]) @=?) $
      parse dValueP "" "TShared<TT>.Create(nil)"
    , testCase "Ensure a parens'ed value parses" $
      (Right (Nil) @=?) $ parse dValueP "" "(nil)"
    , testCase "Ensure a trivial reference value works" $
      (Right (SimpleReference "one") @=?) $ parse dValueP "" "one"
    , testCase "Ensure an <> comparison works" $
      (Right (Operation (SimpleReference "one") "<>" (SimpleReference "two")) @=?) $
      parse dValueP "" "one <> two"
    , testCase "Ensure an and comparison works" $
      (Right (Operation (SimpleReference "one") "and" (SimpleReference "two")) @=?) $
      parse dValueP "" "one and two"
    , testCase "Ensure an as cast works" $
      (Right (Operation (SimpleReference "one") "as" (SimpleReference "two")) @=?) $
      parse dValueP "" "one as two"
    , testCase "Ensure a < works" $
      (Right (Operation (SimpleReference "one") "<" (SimpleReference "two")) @=?) $
      parse dValueP "" "one<two"
    , testCase "Ensure dot operator works" $
      (Right (TypeMemberRef (Type "one") (Type "two") []) @=?) $
      parse dValueP "" "one.two"
    , testCase "Ensure a simple function call works" $
      (Right (FunctionCall (SimpleReference "one") []) @=?) $
      parse dValueP "" "one()"
    , testCase "Ensure a simple function call with one arg works" $
      (Right (FunctionCall (SimpleReference "one") [SimpleReference "two"]) @=?) $
      parse dValueP "" "one(two)"
    , testCase "Ensure a simple function call with two args works" $
      (Right
         (FunctionCall
            (SimpleReference "one")
            [SimpleReference "two", SimpleReference "three"]) @=?) $
      parse dValueP "" "one(two, three)"
    , testCase "Ensure that the empty statement works" $
      (Right EmptyExpression @=?) $ parse dStatementP "" ";"
    , testCase "Ensure that if-function-then works" $
      (Right
         (If
            (FunctionCall
               (SimpleReference "Assigned")
               [SimpleReference "FObjectToFree"])
            (Then EmptyExpression)) @=?) $
      parse dIfExpression "" "if Assigned(FObjectToFree) then ;"
    , testCase "Ensure that ifThenElse works" $
      (Right
         (If
            (Operation
               (Operation (SimpleReference "FFreeTheValue") "<>" Nil)
               "and"
               (Operation
                  (MemberAccess
                     (Operation
                        (SimpleReference "FFreeTheValue")
                        "as"
                        (SimpleReference "TFreeTheValue"))
                     (SimpleReference "FObjectToFree"))
                  "<>"
                  Nil))
            (Then
               (Assign
                  (SimpleReference "Result")
                  (Operation
                     (MemberAccess
                        (Operation
                           (SimpleReference "FFreeTheValue")
                           "as"
                           (SimpleReference "TFreeTheValue"))
                        (SimpleReference "FObjectToFree"))
                     "as"
                     (SimpleReference "T"))))) @=?) $
      parse
        dStatementP
        ""
        "if ( FFreeTheValue <> nil) and ((FFreeTheValue as TFreeTheValue).FObjectToFree <> nil) then Result := (FFreeTheValue as TFreeTheValue).FObjectToFree as T;"
    , testCase "Ensure assign to an index property parses" $
      (Right
         (Assign
            (IndexCall (SimpleReference "Foo") [Integer 32])
            (SimpleReference "blah")) @=?) $
      parse dStatementP "" "Foo[32] := blah;"
    , testCase "Ensure reading an index property parses" $
      (Right (ExpressionValue (IndexCall (SimpleReference "Foo") [Integer 32])) @=?) $
      parse dStatementP "" "Foo[32];"
    , testCase "Ensure that foo.bar.baz.fuux is left associative" $
      (Right
         (MemberAccess
            (MemberAccess
               (MemberAccess (SimpleReference "foo") (SimpleReference "bar"))
               (SimpleReference "baz"))
            (SimpleReference "fuux")) @=?) $
      parse dValueP "" "foo.bar.baz.fuux"
    , testCase "Ensure a.b(c) syntax parses" $
      (Right [Integer 32] @=?) $ parse dIndexArgs "" "a.b(c)"
    , testCase "Ensure a.b[c] syntax parses" $
      (Right [Integer 32] @=?) $ parse dIndexArgs "" "a.b[c]"
    , testCase "Ensure index args syntax parses" $
      (Right [Integer 32] @=?) $ parse dIndexArgs "" "[32]"
    , testCase "Ensure index property parses" $
      (Right (IndexCall (SimpleReference "Foo") [Integer 32]) @=?) $
      parse dValueP "" "Foo[32]"
    ]
