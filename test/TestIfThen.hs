{-# LANGUAGE OverloadedStrings #-}

module TestIfThen ( ifThenTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (dIfExpression, statement)
import Text.Megaparsec (parse)

v a = V $ Lexeme "" a

ifThenTests :: TestTree
ifThenTests = testGroup
  "Delphi if/then/else tests"
  [ testCase "Ensure that if-function-then works"
  $ (Right
      (If (v "Assigned" :$ [v "FObjectToFree"])
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
        (P [v "FFreeTheValue" :<> Nil] :& P
          [ (  P [As (v "FFreeTheValue") (v "TFreeTheValue")]
            :. v "FObjectToFree"
            )
              :<> Nil
          ]
        )
        (Then
          (v "Result" := As
            (  P [As (v "FFreeTheValue") (v "TFreeTheValue")]
            :. v "FObjectToFree"
            )
            (v "T")
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
