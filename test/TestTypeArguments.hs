{-# LANGUAGE OverloadedStrings #-}

module TestTypeArguments
  ( typeArgumentTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import DelphiParser (typeArguments')
import Text.Megaparsec (parse)

import DelphiAst

import Data.Maybe (Maybe(Just))

typeArgumentTests :: TestTree
typeArgumentTests =
  testGroup
    "Delphi Type Argument Tests"
    [ testCase "Zero arguments, not even parens" $
      (Right Nothing @=?) $ parse typeArguments' "" ""
    , testCase "Zero arguments, but has parens" $
      (Right (Just []) @=?) $ parse typeArguments' "" "()"
    , testCase "One simple argument" $
      (Right (Just [Arg NormalArg "AFoo" (Just $ Type "TBar") Nothing]) @=?) $
      parse typeArguments' "" "(AFoo: TBar)"
    , testCase "One 'default' argument" $
      (Right (Just [Arg ConstArg "Default" (Just $ Type "TBar") Nothing]) @=?) $
      parse typeArguments' "" "(const Default: TBar)"
    , testCase "One argument with default" $
      (Right (Just [Arg ConstArg "aFoo" (Just $ Type "TFoo") (Just (I 32))]) @=?) $
      parse typeArguments' "" "(aFoo: TFoo = 32)"
    , testCase "One argument with default array" $
      (Right (Just [Arg ConstArg "aFoo" (Just $ Type "TFoo") (Just $ L [I 32, I 33])]) @=?) $
      parse typeArguments' "" "(aFoo: TFoo = [32, 33])"
    , testCase "One const 'array of const' argument" $
      (Right (Just [Arg ConstArg "Default" (Just $ DynamicArray 1 ConstType) Nothing]) @=?) $
      parse typeArguments' "" "(const Default: array of const)"
    , testCase "Two simple argument" $
      (Right
         (Just
            [ Arg NormalArg "AFoo" (Just $ Type "TBar") Nothing
            , Arg NormalArg "ABaz" (Just $ Type "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo: TBar; ABaz: TFuux)"
    , testCase "Two simple argument, with shorthand" $
      (Right
         (Just
            [ Arg NormalArg "AFoo" (Just $ Type "TFuux") Nothing
            , Arg NormalArg "ABaz" (Just $ Type "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo, ABaz: TFuux)"
    , testCase "Ensure that 'class' is a valid argument type" $
      (Right (Just [Arg NormalArg "foo" (Just $ Type "class") Nothing]) @=?) $
      parse typeArguments' "" "(foo: class)"
    , testCase "Ensure that 'const' is a valid" $
      (Right (Just [Arg ConstArg "foo" (Just $ Type "TFoo") Nothing]) @=?) $
      parse typeArguments' "" "(const foo: TFoo)"
    , testCase "Slightly complex example" $
      (Right
         (Just
            [ Arg NormalArg "foo" (Just $ Type "TFoo") Nothing
            , Arg NormalArg "bar" (Just $ Type "TFoo") Nothing
            , Arg ConstArg "baz" (Just $ Type "TBaz") Nothing
            ]) @=?) $
      parse typeArguments' "" "(foo, bar: TFoo; const baz: TBaz)"
    ]
