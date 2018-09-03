{-# LANGUAGE OverloadedStrings #-}

module TestProcedureImplementation ( procedureImplementationTest ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import DelphiAst
import DelphiParser (procedureImpl, functionImpl)
import Text.Megaparsec (parse)

procedureImplementationTest :: TestTree
procedureImplementationTest = testGroup
  "Delphi Procedure and Function Implementation Tests"
  [ testGroup
    "Procedure Implementation Tests"
    [ testCase "Regular Procedure - simplest case"
    $ (Right (ProcedureImpl (Type "foo") [] [] [] (Begin [])) @=? )
    $ parse procedureImpl "" "procedure foo;\nbegin end;"
    , testCase "Nested procedure"
    $ (Right (ProcedureImpl (Type "foo") [] [] [] (Begin [])) @=? )
    $ parse procedureImpl "" "procedure foo;\nprocedure bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested procedure after vars"
    $ (Right (ProcedureImpl (Type "foo") [] [] [] (Begin [])) @=? )
    $ parse procedureImpl "" "procedure foo;\nvar fuux: TFuux;\n\nprocedure bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested procedures (multiple) after vars"
    $ (Right (ProcedureImpl (Type "foo") [] [] [] (Begin [])) @=? )
    $ parse procedureImpl "" "procedure foo;\nvar fuux: TFuux;\n\nprocedure bar;\nbegin\nend;\nprocedure alpha;\nbegin\nend;\nbegin\nend;"
    ]
  , testGroup
    "Function Implementation Tests"
    [ testCase "Regular Function - simplest case"
    $ (Right (FunctionImpl (Type "foo") [] (Type "bar") [] [] (Begin [])) @=? )
    $ parse functionImpl "" "function foo: bar;\nbegin end;"
    , testCase "Nested function"
    $ (Right (FunctionImpl (Type "foo") [] (Type "bar") [] [] (Begin [])) @=? )
    $ parse functionImpl "" "function foo: bar;\nfunction bar:bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested function after vars"
    $ (Right (FunctionImpl (Type "foo") [] (Type "bar") [] [] (Begin [])) @=? )
    $ parse functionImpl "" "function foo: bar;\nvar fuux: TFuux;\n\nfunction bar:bar;\nbegin\nend;\nbegin\nend;"
    , testCase "Nested functions (multiple) after vars"
    $ (Right (FunctionImpl (Type "foo") [] (Type "bar") [] [] (Begin [])) @=? )
    $ parse functionImpl "" "function foo: bar;\nvar fuux: TFuux;\n\nfunction bar:bar;\nbegin\nend;\nfunction alpha:beta;\nbegin\nend;\nbegin\nend;"
    ]
  ]
