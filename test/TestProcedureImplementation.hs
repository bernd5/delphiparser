{-# LANGUAGE OverloadedStrings #-}

module TestProcedureImplementation ( procedureImplementationTest ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (procedureImpl, functionImpl)
import Text.Megaparsec (parse)

v a = V $ Lexeme "" a
typ a = Type $ Lexeme "" a
varDefinition a b c = VarDefinition (Lexeme "" a) b c

procedureImplementationTest :: TestTree
procedureImplementationTest = testGroup
  "Delphi Procedure and Function Implementation Tests"
  [ testGroup
    "Procedure Implementation Tests"
    [ testCase "Regular Procedure - simplest case"
    $ (Right (ProcedureImpl (typ "foo") [] [] [] (Begin [])) @=?)
    $ parse procedureImpl "" "procedure foo;\nbegin end;"
    , testCase "Nested procedure"
    $ (Right
        (ProcedureImpl (typ "foo")
                       []
                       []
                       [ProcedureImpl (typ "bar") [] [] [] (Begin [])]
                       (Begin [])
        ) @=?
      )
    $ parse procedureImpl
            ""
            "procedure foo;\nprocedure bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested procedure after vars"
    $ (Right
        (ProcedureImpl
          (typ "foo")
          []
          []
          [ AdditionalInterface
            $ VarDefinitions [varDefinition "fuux" (typ $ "TFuux") Nothing]
          , ProcedureImpl (typ "bar") [] [] [] (Begin [])
          ]
          (Begin [])
        ) @=?
      )
    $ parse
        procedureImpl
        ""
        "procedure foo;\nvar fuux: TFuux;\n\nprocedure bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested procedures (multiple) after vars"
    $ (Right
        (ProcedureImpl
          (typ "foo")
          []
          []
          [ AdditionalInterface
            $ VarDefinitions [varDefinition "fuux" (typ "TFuux") Nothing]
          , ProcedureImpl (typ $ "bar") [] [] [] (Begin [])
          , ProcedureImpl (typ "alpha") [] [] [] (Begin [])
          ]
          (Begin [])
        ) @=?
      )
    $ parse
        procedureImpl
        ""
        "procedure foo;\nvar fuux: TFuux;\n\nprocedure bar;\nbegin\nend;\nprocedure alpha;\nbegin\nend;\nbegin\nend;"
    ]
  , testGroup
    "Function Implementation Tests"
    [ testCase "Regular Function - simplest case"
    $ (Right (FunctionImpl (typ "foo") [] (typ "bar") [] [] (Begin [])) @=?)
    $ parse functionImpl "" "function foo: bar;\nbegin end;"
    , testCase "Nested function"
    $ (Right
        (FunctionImpl
          (typ "foo")
          []
          (typ "bar")
          []
          [FunctionImpl (typ "bar") [] (typ "bar") [] [] (Begin [])]
          (Begin [])
        ) @=?
      )
    $ parse functionImpl
            ""
            "function foo: bar;\nfunction bar:bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested function after vars"
    $ (Right
        (FunctionImpl
          (typ "foo")
          []
          (typ "bar")
          []
          [ AdditionalInterface
            (VarDefinitions [varDefinition "fuux" (typ "TFuux") Nothing])
          , FunctionImpl (typ "bar") [] (typ "bar") [] [] (Begin [])
          ]
          (Begin [])
        ) @=?
      )
    $ parse
        functionImpl
        ""
        "function foo: bar;\nvar fuux: TFuux;\n\nfunction bar:bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested functions (multiple) after vars"
    $ (Right
        (FunctionImpl
          (typ "foo")
          []
          (typ "bar")
          []
          [ AdditionalInterface
            (VarDefinitions [varDefinition "fuux" (typ "TFuux") Nothing])
          , FunctionImpl (typ "bar")   [] (typ "bar")  [] [] (Begin [])
          , FunctionImpl (typ "alpha") [] (typ "beta") [] [] (Begin [])
          ]
          (Begin [])
        ) @=?
      )
    $ parse
        functionImpl
        ""
        "function foo: bar;\nvar fuux: TFuux;\n\nfunction bar:bar;\nbegin\nend;\nfunction alpha:beta;\nbegin\nend;\nbegin\nend;"
    , testCase "Trivial procedure with assignment"
    $ (Right
        (ProcedureImpl
          (typ "foo") [] [] [] (Begin [v "a" := v "b"])) @=? )
    $ parse procedureImpl "" "procedure foo; begin a := b end;"
    , testCase "Trivial procedure with assignment with semicolon"
    $ (Right
        (ProcedureImpl
          (typ "foo") [] [] [] (Begin [v "a" := v "b"])) @=? )
    $ parse procedureImpl "" "procedure foo; begin a := b; end;"

    ]
  ]
