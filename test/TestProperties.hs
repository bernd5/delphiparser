{-# LANGUAGE OverloadedStrings #-}

module TestProperties ( propertiesTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (property')

import DelphiAst
import Data.Maybe (Maybe(Just))

property a b c d e f = Property (Lexeme "" a) b c d e f
arg a b c d = Arg a (Lexeme "" b) c d
typ a = Type $ Lexeme "" a
propertyRead a = PropertyRead (map (\x -> Lexeme "" x) a)
propertyWrite a = PropertyWrite (map (\x -> Lexeme "" x) a)
genericInstance a b = GenericInstance (Lexeme "" a) b

propertiesTests :: TestTree
propertiesTests = testGroup "Delphi Property Tests"
  [ testCase "property Items[I: Integer]: T read GetItem" $
    (Right (property "Items" (Just [arg NormalArg "I" (Just $ typ "Integer") Nothing]) (typ "T") Nothing [propertyRead ["GetItem"]] False) @=? )$
    parse property' "" "property Items[I: Integer]: T read GetItem;"
  , testCase "property Items[I: Integer]: T<E> read GetItem" $
    (Right (property "Items" (Just [arg NormalArg "I" (Just $ typ "Integer") Nothing]) (genericInstance "T" [typ "E"]) Nothing [propertyRead ["GetItem"]] True) @=? )$
    parse property' "" "property Items[I: Integer]: T<E> read GetItem; default;"
  , testCase "property Left: Integer read FLeft write FLeft default []" $
    (Right (property "Left" Nothing (typ "Integer") Nothing [propertyRead ["FLeft"], propertyWrite ["FLeft"], PropertyDefault (L [])] False) @=? ) $
    parse property' "" "property Left: Integer read FLeft write FLeft default [];"
  , testCase "property Left: Integer read FLeft write FLeft default 0" $
    (Right (property "Left" Nothing (typ "Integer") Nothing [propertyRead ["FLeft"], propertyWrite ["FLeft"], PropertyDefault (I $ Lexeme "" 0)] False) @=? ) $
    parse property' "" "property Left: Integer read FLeft write FLeft default 0;"
  , testCase "property Left: Integer index 2 read GetLeft write SetLeft" $
    (Right (property "Left" Nothing (typ "Integer") (Just $ I $ Lexeme "" 2) [propertyRead ["GetLeft"], propertyWrite ["GetLeft"]] False) @=? ) $
    parse property' "" "property Left: Integer index 2 read GetLeft write GetLeft;"
  ]
