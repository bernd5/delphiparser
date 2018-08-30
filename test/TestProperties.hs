{-# LANGUAGE OverloadedStrings #-}

module TestProperties ( propertiesTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (property')

import DelphiAst

propertiesTests :: TestTree
propertiesTests = testGroup "Delphi Property Tests"
  [ testCase "property Items[I: Integer]: T read GetItem" $
    (Right (Property "Items" (Just [Arg "I" (Type "Integer")]) (Type "T") Nothing [PropertyRead "GetItem"] False) @=? )$
    parse property' "" "property Items[I: Integer]: T read GetItem;"
  , testCase "property Items[I: Integer]: T<E> read GetItem" $
    (Right (Property "Items" (Just [Arg "I" (Type "Integer")]) (GenericInstance "T" [Type "E"]) Nothing [PropertyRead "GetItem"] True) @=? )$
    parse property' "" "property Items[I: Integer]: T<E> read GetItem; default;"
  ]
