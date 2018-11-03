{-# LANGUAGE OverloadedStrings #-}

module TestIfThen ( ifThenTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (dIfExpression, statement)
import Text.Megaparsec (parse)

ifThenTests :: TestTree
ifThenTests = testGroup
  "Delphi if/then/else tests"
  [ testCase "Ensure that if-function-then works"
  $ (Right
      (If (V "Assigned" :$ [V "FObjectToFree"])
          (Then EmptyExpression)
          (Else EmptyExpression)
      ) @=?
    )
  $ parse dIfExpression "" "if Assigned(FObjectToFree) then ;"
  , testCase "Ensure that an empty ifThenElse works with both then and else"
  $ (Right (If Nil (Then EmptyExpression) (Else EmptyExpression)) @=? )
  $ parse statement "" "if nil then else;"
  , testCase "Ensure that ifThenElse works"
  $ (Right
      (If
        (P [V "FFreeTheValue" :<> Nil] :& P
          [ (  P [As (V "FFreeTheValue") (V "TFreeTheValue")]
            :. V "FObjectToFree"
            )
              :<> Nil
          ]
        )
        (Then
          (V "Result" := As
            (  P [As (V "FFreeTheValue") (V "TFreeTheValue")]
            :. V "FObjectToFree"
            )
            (V "T")
          )
        )
        (Else EmptyExpression)
      ) @=?
    )
  $ parse
      statement
      ""
      "if ( FFreeTheValue <> nil) and ((FFreeTheValue as TFreeTheValue).FObjectToFree <> nil) then Result := (FFreeTheValue as TFreeTheValue).FObjectToFree as T;"
  ]
