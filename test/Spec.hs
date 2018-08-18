{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Data.Text
import Data.Text.IO (readFile)

import Test.Tasty
import Test.Tasty.HUnit

import Text.Megaparsec (parse)
import DelphiLexer
import DelphiAst
import DelphiParser

data ParserTestsData = ParserTestsData {
  sharedpointer :: Text 
}

main :: IO ()
main = do
  sp <- readFile "deps/sharedpointer.pas"
  let p = ParserTestsData {sharedpointer = sp}
  defaultMain $ testGroup "Tests" [unitTests p]

unitTests p = testGroup "Delphi Parser Tests"
  [ testCase "Ensure delphi skeleton parses" $
    (Right (
      Unit "TestUnit" (Interface []) (Implementation []) Initialization Finalization )
     @=?) $
    parse dUnitP "testUnit.pas"  "unit TestUnit; interface type implementation initialization finalization end."

  , testCase "Ensure a Generic Member of a Generic Class with a Generic Result parses" $
    (Right (
      MemberFunctionImpl (GenericInstance "TShared" [Type "T"])
                         (GenericInstance "Cast" [Type "TT"])
                         []
                         (GenericInstance "TShared" [Type "TT"])
     (Begin [])) @=?) $
    parse dFunctionImplementationP "" $ unpack $ intercalate "\n" [
      "function TShared<T>.Cast<TT>: TShared<TT>;",
      "begin",
      "end;"]

  , testCase "Ensure a Generic Procedure of a Generic Class parses" $
    (Right (
      MemberProcedureImpl (GenericInstance "TShared" [Type "T"])
                         (GenericInstance "Cast" [Type "TT"])
                         []
                         (Begin [])
    ) @=?) $
    parse dProcedureImplementationP "" $ unpack $ intercalate "\n" [
      "procedure TShared<T>.Cast<TT>;",
      "begin",
      "end;"]

  , testCase "Ensure a simple function call parses using dStatementP" $
    (Right (ExpressionValue (FunctionCall (SimpleReference "SetLength") [SimpleReference "FList", SimpleReference "FList"]))
      @=?) $
    parse dStatementP "" "SetLength(FList, FList);"

  , testCase "Ensure a nested function call parses" $
    (Right (ExpressionValue (FunctionCall (SimpleReference "SetLength") [SimpleReference "FList",FunctionCall (SimpleReference "Length") [SimpleReference "FList"]]))
      @=?) $
    parse dStatementP "" "SetLength(FList, Length(FList));"

  , testCase "Ensure a slightly complex nested function call parses" $
    ( Right (ExpressionValue (
        FunctionCall (SimpleReference "SetLength") [
          SimpleReference "FList",
          Operation (
            FunctionCall (SimpleReference "Length") [
              SimpleReference "FList"])
            "+" (Integer 1)]))
      @=?) $
    parse dStatementP "" "SetLength(FList, Length(FList)+1);"

  , testCase "Ensure that you can assign to the result of a cast" $
    (Right (Assign (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (SimpleReference "FObjectToFree")) Nil)
      @=? ) $
    parse dStatementP "" "(FFreeTheValue as TFreeTheValue).FObjectToFree := nil;"

  , testCase "Ensure a value involving a generic type member function parses" $
      (Right
         (TypeMemberRef (GenericInstance "TShared" [Type "TT"]) 
                        (Type "Create" )
                        [Nil])
         @=?) $
    parse dValueP "" "TShared<TT>.Create(nil)"

  , testCase "Ensure a parens'ed value parses" $
     (Right (Nil) @=?) $
    parse dValueP "" "(nil)"

  , testCase "Ensure a trivial reference value works"  $
     (Right (SimpleReference "one") @=?) $
    parse dValueP "" "one"

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
     (Right (TypeMemberRef (Type "one") 
                           (Type "two") [])
       @=?) $
    parse dValueP "" "one.two"

  , testCase "Ensure a simple function call works" $
     (Right (FunctionCall (SimpleReference "one") [])
        @=?) $
    parse dValueP "" "one()"

  , testCase "Ensure a simple function call with one arg works" $
     (Right (FunctionCall (SimpleReference "one") [SimpleReference "two"])
        @=?) $
    parse dValueP "" "one(two)"

  , testCase "Ensure a simple function call with two args works" $
     (Right (FunctionCall (SimpleReference "one") [SimpleReference "two",SimpleReference "three"])
        @=?) $
    parse dValueP "" "one(two, three)"

  , testCase "Ensure that the empty statement works" $
    (Right EmptyExpression
      @=?) $ parse dStatementP "" ";"

  , testCase "Ensure that if-function-then works" $
    (Right (If (FunctionCall (SimpleReference "Assigned") [SimpleReference "FObjectToFree"]) (Then EmptyExpression))
      @=?) $
    parse dIfExpression "" "if Assigned(FObjectToFree) then ;"

  , testCase "Ensure that ifThenElse works" $
    (Right (If (Operation (Operation (SimpleReference "FFreeTheValue") "<>" Nil) "and" (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (Operation (SimpleReference "FObjectToFree") "<>" Nil))) (Then (Assign (SimpleReference "Result") (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (Operation (SimpleReference "FObjectToFree") "as" (SimpleReference "T")))))) @=?) $
    parse dStatementP "" "if ( FFreeTheValue <> nil) and ((FFreeTheValue as TFreeTheValue).FObjectToFree <> nil) then Result := (FFreeTheValue as TFreeTheValue).FObjectToFree as T;"

  , testCase "Ensure assign to an index property parses" $
    (Right (Assign (IndexCall (SimpleReference "Foo") [Integer 32]) (SimpleReference "blah"))
      @=?) $
      parse dStatementP "" "Foo[32] := blah;"

  , testCase "Ensure reading an index property parses" $
    (Right ( ExpressionValue (IndexCall (SimpleReference "Foo") [Integer 32] ))
      @=?) $
      parse dStatementP "" "Foo[32];"

  , testCase "Ensure index args syntax parses" $
    ( Right [Integer 32]
      @=?) $
      parse dIndexArgs "" "[32]"

  , testCase "Ensure index property parses" $
    (Right (IndexCall (SimpleReference "Foo") [Integer 32] )
      @=?) $
      parse dValueP "" "Foo[32]"

  , testCase "Ensure reading a real unit parses" $
    (Right (Unit "SharedPointer" (Interface [TypeDef (Type "TDeallocator") (ReferenceToProcedure [Arg "AObj" (Type "TObject")]),Record (GenericDefinition "TShared" [Arg "T" (Type "class")]) [Private [Field "FFreeTheValue" (Type "IInterface")],Public [Constructor "Create" [Arg "AValue" (Type "T")],Procedure "Assign" [Arg "AValue" (Type "T")] [],Procedure "SetDeallocator" [Arg "ADealloc" (Type "TDeallocator")] [],Function "Temporary" [] (Type "T") [],Function "Cast" [] (GenericInstance "TShared" [Type "TT"]) [],Function "Release" [] (Type "T") []]],Record (GenericDefinition "TFreeTheValue" []) [Public [Field "FObjectToFree" (Type "TObject"),Field "FCustomDeallocator" (Type "TDeallocator"),Constructor "Create" [Arg "AObjToFree" (Type "TObject")],Destructor "Destroy" [Override]]],Record (GenericDefinition "TSharedList" [Arg "T" (Type "class")]) [Private [Field "FList" (GenericInstance "TShared" [Type "T"]),Function "GetItem" [Arg "I" (Type "Integer")] (Type "T") [],Function "GetSharedItem" [Arg "I" (Type "Integer")] (GenericInstance "TShared" [Type "T"]) []],Public [Function "Count" [] (Type "Integer") [],IndexProperty "Items" (Arg "I" (Type "Integer")) (Type "T") (Just "GetItem") Nothing [],IndexProperty "SharedItems" (Arg "I" (Type "Integer")) (GenericInstance "TShared" [Type "T"]) (Just "GetSharedItem") Nothing [Default],Procedure "Add" [Arg "AObject" (GenericInstance "TShared" [Type "T"])] [],Procedure "Clear" [] []]]]) (Implementation [MemberFunctionImpl (GenericInstance "TShared" [Type "T"]) (Type "Temporary") [] (Type "T") (Begin [Assign (SimpleReference "Result") Nil,If (Operation (Operation (SimpleReference "FFreeTheValue") "<>" Nil) "and" (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (Operation (SimpleReference "FObjectToFree") "<>" Nil))) (Then (Assign (SimpleReference "Result") (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (Operation (SimpleReference "FObjectToFree") "as" (SimpleReference "T")))))]),MemberConstructorImpl (Type "TFreeTheValue") (Type "Create") [Arg "AObjToFree" (Type "TObject")] (Begin [Assign (SimpleReference "FObjectToFree") (SimpleReference "AObjToFree")]),MemberDestructorImpl (Type "TFreeTheValue") (Type "Destroy") [] (Begin [If (FunctionCall (SimpleReference "Assigned") [SimpleReference "FObjectToFree"]) (Then (Begin [If (FunctionCall (SimpleReference "Assigned") [SimpleReference "FCustomDeallocator"]) (Then (Begin [ExpressionValue (FunctionCall (SimpleReference "FCustomDeallocator") [SimpleReference "FObjectToFree"]),Assign (SimpleReference "FObjectToFree") Nil,Assign (SimpleReference "FCustomDeallocator") Nil]))]))]),MemberFunctionImpl (GenericInstance "TShared" [Type "T"]) (GenericInstance "Cast" [Type "TT"]) [] (GenericInstance "TShared" [Type "TT"]) (Begin [Assign (SimpleReference "Result") (TypeMemberRef (GenericInstance "TShared" [Type "TT"]) (Type "Create") [Nil]),Assign (TypeMemberRef (Type "Result") (Type "FFreeTheValue") []) (SimpleReference "FFreeTheValue")]),MemberConstructorImpl (GenericInstance "TShared" [Type "T"]) (Type "Create") [Arg "AValue" (Type "T")] (Begin [ExpressionValue (FunctionCall (SimpleReference "Assign") [SimpleReference "AValue"])]),MemberProcedureImpl (GenericInstance "TShared" [Type "T"]) (Type "Assign") [Arg "AValue" (Type "T")] (Begin [Assign (SimpleReference "FFreeTheValue") (TypeMemberRef (Type "TFreeTheValue") (Type "Create") [SimpleReference "AValue"])]),MemberFunctionImpl (GenericInstance "TShared" [Type "T"]) (Type "Release") [] (Type "T") (Begin [Assign (SimpleReference "Result") (SimpleReference "Temporary"),Assign (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (SimpleReference "FObjectToFree")) Nil]),MemberProcedureImpl (GenericInstance "TShared" [Type "T"]) (Type "SetDeallocator") [Arg "ADealloc" (Type "TDeallocator")] (Begin [Assign (Operation (Operation (SimpleReference "FFreeTheValue") "as" (SimpleReference "TFreeTheValue")) "." (SimpleReference "FCustomDeallocator")) (SimpleReference "ADealloc")]),MemberProcedureImpl (GenericInstance "TSharedList" [Type "T"]) (Type "Add") [Arg "AObject" (GenericInstance "TShared" [Type "T"])] (Begin [ExpressionValue (FunctionCall (SimpleReference "SetLength") [SimpleReference "FList",Operation (FunctionCall (SimpleReference "Length") [SimpleReference "FList"]) "+" (Integer 1)]),Assign (IndexCall (SimpleReference "FList") [Operation (FunctionCall (SimpleReference "Length") [SimpleReference "FList"]) "-" (Integer 1)]) (SimpleReference "AObject")]),MemberProcedureImpl (GenericInstance "TSharedList" [Type "T"]) (Type "Clear") [] (Begin [ExpressionValue (FunctionCall (SimpleReference "SetLength") [SimpleReference "FList",Integer 0])]),MemberFunctionImpl (GenericInstance "TSharedList" [Type "T"]) (Type "Count") [] (Type "Integer") (Begin [Assign (SimpleReference "Result") (FunctionCall (SimpleReference "Length") [SimpleReference "FList"])]),MemberFunctionImpl (GenericInstance "TSharedList" [Type "T"]) (Type "GetSharedItem") [Arg "I" (Type "Integer")] (GenericInstance "TShared" [Type "T"]) (Begin [Assign (SimpleReference "Result") (IndexCall (SimpleReference "FList") [SimpleReference "I"])]),MemberFunctionImpl (GenericInstance "TSharedList" [Type "T"]) (Type "GetItem") [Arg "I" (Type "Integer")] (Type "T") (Begin [Assign (SimpleReference "Result") (Operation (FunctionCall (SimpleReference "GetSharedItem") [SimpleReference "I"]) "." (SimpleReference "Temporary"))])]) Initialization Finalization)
     @=?) $
    parse dUnitP "sharedpointer.pas" (unpack $ sharedpointer p)
  ]
