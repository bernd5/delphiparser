{-# LANGUAGE OverloadedStrings #-}

module TestProperties ( propertiesTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (property')

import DelphiAst
import Data.Maybe (Maybe(Just))

propertiesTests :: TestTree
propertiesTests = testGroup "Delphi Property Tests"
  [ testCase "property Items[I: Integer]: T read GetItem" $
    (Right (Property "Items" (Just [Arg NormalArg "I" (Just $ Type "Integer") Nothing]) (Type "T") Nothing [PropertyRead ["GetItem"]] False) @=? )$
    parse property' "" "property Items[I: Integer]: T read GetItem;"
  , testCase "property Items[I: Integer]: T<E> read GetItem" $
    (Right (Property "Items" (Just [Arg NormalArg "I" (Just $ Type "Integer") Nothing]) (GenericInstance "T" [Type "E"]) Nothing [PropertyRead ["GetItem"]] True) @=? )$
    parse property' "" "property Items[I: Integer]: T<E> read GetItem; default;"
  , testCase "property Left: Integer read FLeft write FLeft default 0" $
    (Right (Property "Left" Nothing (Type "Integer") Nothing [PropertyRead ["FLeft"], PropertyWrite ["FLeft"], PropertyDefault (I 0)] False) @=? ) $
    parse property' "" "property Left: Integer read FLeft write FLeft default 0;"
  , testCase "property Left: Integer index 2 read GetLeft write SetLeft" $
    (Right (Property "Left" Nothing (Type "Integer") (Just $ I 2) [PropertyRead ["GetLeft"], PropertyWrite ["GetLeft"]] False) @=? ) $
    parse property' "" "property Left: Integer index 2 read GetLeft write GetLeft;"
  ]
