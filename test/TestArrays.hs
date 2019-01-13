{-# LANGUAGE OverloadedStrings #-}

module TestArrays ( arrayTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (array', typeName)

import DelphiAst

arrayTests :: TestTree
arrayTests = testGroup "Delphi Array Tests"
  [ testGroup "Static Arrays" [
      testCase "array [foo] of bar" $
      (Right (StaticArray (IndexOf [V $ Lexeme "" "foo"]) (Type $ Lexeme "" "bar")) @=?) $
      parse array' "" "array [foo] of bar"
    , testCase "array [foo] of bar[32]" $
      (Right (StaticArray (IndexOf [V $ Lexeme "" "foo"]) (Type $ Lexeme "" "bar")) @=?) $
      parse typeName "" "array [foo] of bar[32]"
    , testCase "array [foo, bar] of baz" $
      (Right (StaticArray (IndexOf [V $ Lexeme "" "foo", V $ Lexeme "" "bar"]) (Type $ Lexeme "" "baz")) @=?) $
      parse array' "" "array [foo, bar] of baz"
    , testCase "array [43..foo-2] of bar" $
      (Right (StaticArray (IndexOf [I (Lexeme "" 43) :.. (V (Lexeme "" "foo") :- I (Lexeme "" 2))]) (Type $ Lexeme "" "bar")) @=?) $
      parse array' "" "array [43..foo-2] of bar"
    , testCase "array [43..25] of bar" $
      (Right (StaticArray (IndexOf [I (Lexeme "" 43) :.. I (Lexeme "" 25)]) (Type $ Lexeme "" "bar")) @=?) $
      parse array' "" "array [43..25] of bar"
    , testCase "array [43..25,5..6] of bar" $
      (Right (StaticArray (IndexOf [I (Lexeme "" 43) :.. I (Lexeme "" 25), I (Lexeme "" 5) :.. I (Lexeme "" 6)]) (Type $ Lexeme "" "bar")) @=?) $
      parse array' "" "array [43..25,5..6] of bar"
    ]
  , testGroup "Dynamic Arrays"
    [ testCase "array of foo" $
      (Right (DynamicArray 1 (Type $ Lexeme "" "foo")) @=?) $
      parse array' "" "array of foo"
    , testCase "array of array of foo" $
      (Right (DynamicArray 2 (Type $ Lexeme "" "foo")) @=?) $
      parse array' "" "array of array of foo"
    ]
  , testGroup "Open Dynamic Arrays" $
    [ testCase "array of const" $
      (Right (DynamicArray 1 ConstType) @=? ) $
      parse array' "" "array of const"
    ]
  , testGroup "Variant Arrays" $
    [ testCase "array [foo] of const" $
      (Right (VariantArray (IndexOf [V $ Lexeme "" "foo"])) @=? ) $
      parse array' "" "array [foo] of const"
    ]
  , testGroup "Open Dynamic Arrays"
    [ testCase "array foo" $
      (Right (OpenDynamicArray $ Type $ Lexeme "" "foo") @=? ) $
      parse array' "" "array foo"
    ]
  ]

