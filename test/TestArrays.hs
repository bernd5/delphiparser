{-# LANGUAGE OverloadedStrings #-}

module TestArrays ( arrayTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (array', typeName)

import DelphiAst

i a = I (Lexeme Empty a)
v a = V (Lexeme Empty a)
typ a = Type (Lexeme Empty a)

arrayTests :: TestTree
arrayTests = testGroup "Delphi Array Tests"
  [ testGroup "Static Arrays" [
      testCase "array [foo] of bar" $
      (Right (StaticArray (IndexOf [v "foo"]) (typ "bar")) @=?) $
      parse array' "" "array [foo] of bar"
    , testCase "array [foo] of bar[32]" $
      (Right (StaticArray (IndexOf [v "foo"]) (StaticArray (IndexOf [i 32]) (typ "bar"))) @=?) $
      parse typeName "" "array [foo] of bar[32]"
    , testCase "array [foo, bar] of baz" $
      (Right (StaticArray (IndexOf [v "foo", v "bar"]) (typ "baz")) @=?) $
      parse array' "" "array [foo, bar] of baz"
    , testCase "array [43..foo-2] of bar" $
      (Right (StaticArray (IndexOf [i 43 :.. (v "foo" :- i 2)]) (typ "bar")) @=?) $
      parse array' "" "array [43..foo-2] of bar"
    , testCase "array [43..25] of bar" $
      (Right (StaticArray (IndexOf [i 43 :.. i 25]) (typ "bar")) @=?) $
      parse array' "" "array [43..25] of bar"
    , testCase "array [43..25,5..6] of bar" $
      (Right (StaticArray (IndexOf [i 43 :.. i 25, i 5 :.. i 6]) (typ "bar")) @=?) $
      parse array' "" "array [43..25,5..6] of bar"
    ]
  , testGroup "Dynamic Arrays"
    [ testCase "array of foo" $
      (Right (DynamicArray 1 (typ "foo")) @=?) $
      parse array' "" "array of foo"
    , testCase "array of array of foo" $
      (Right (DynamicArray 2 (typ "foo")) @=?) $
      parse array' "" "array of array of foo"
    ]
  , testGroup "Open Dynamic Arrays" $
    [ testCase "array of const" $
      (Right (DynamicArray 1 ConstType) @=? ) $
      parse array' "" "array of const"
    ]
  , testGroup "Variant Arrays" $
    [ testCase "array [foo] of const" $
      (Right (VariantArray (IndexOf [v "foo"])) @=? ) $
      parse array' "" "array [foo] of const"
    ]
  , testGroup "Open Dynamic Arrays"
    [ testCase "array foo" $
      (Right (OpenDynamicArray $ typ "foo") @=? ) $
      parse array' "" "array foo"
    ]
  ]

