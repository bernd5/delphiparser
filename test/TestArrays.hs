{-# LANGUAGE OverloadedStrings #-}

module TestArrays ( arrayTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (array')

import DelphiAst

arrayTests :: TestTree
arrayTests = testGroup "Delphi Array Tests"
  [ testGroup "Static Arrays" [
      testCase "array [foo] of bar" $
      (Right (StaticArray (IndexOf [V "foo"]) (Type "bar")) @=?) $
      parse array' "" "array [foo] of bar"
    , testCase "array [foo, bar] of baz" $
      (Right (StaticArray (IndexOf [V "foo", V "bar"]) (Type "baz")) @=?) $
      parse array' "" "array [foo, bar] of baz"
    , testCase "array [43..foo-2] of bar" $
      (Right (StaticArray (IndexOf [I 43 :.. (V "foo" :- I 2)]) (Type "bar")) @=?) $
      parse array' "" "array [43..foo-2] of bar"
    , testCase "array [43..25] of bar" $
      (Right (StaticArray (IndexOf [I 43 :.. I 25]) (Type "bar")) @=?) $
      parse array' "" "array [43..25] of bar"
    , testCase "array [43..25,5..6] of bar" $
      (Right (StaticArray (IndexOf [I 43 :.. I 25, I 5 :.. I 6]) (Type "bar")) @=?) $
      parse array' "" "array [43..25,5..6] of bar"
    ]
  , testGroup "Dynamic Arrays"
    [ testCase "array of foo" $
      (Right (DynamicArray 1 (Type "foo")) @=?) $
      parse array' "" "array of foo"
    , testCase "array of array of foo" $
      (Right (DynamicArray 2 (Type "foo")) @=?) $
      parse array' "" "array of array of foo"
    ]
  , testGroup "Open Dynamic Arrays" $
    [ testCase "array of const" $
      (Right (DynamicArray 1 ConstType) @=? ) $
      parse array' "" "array of const"
    ]
  , testGroup "Variant Arrays" $
    [ testCase "array [foo] of const" $
      (Right (VariantArray (IndexOf [V "foo"])) @=? ) $
      parse array' "" "array [foo] of const"
    ]
  , testGroup "Open Dynamic Arrays"
    [ testCase "array foo" $
      (Right (OpenDynamicArray $ Type "foo") @=? ) $
      parse array' "" "array foo"
    ]
  ]

