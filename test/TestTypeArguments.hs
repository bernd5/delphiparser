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

arg a b c d = Arg a (Lexeme "" b) c d
typ a = Type $ Lexeme "" a

typeArgumentTests :: TestTree
typeArgumentTests =
  testGroup
    "Delphi typ Argument Tests"
    [ testCase "Zero arguments, not even parens" $
      (Right Nothing @=?) $ parse typeArguments' "" ""
    , testCase "Zero arguments, but has parens" $
      (Right (Just []) @=?) $ parse typeArguments' "" "()"
    , testCase "One simple argument" $
      (Right (Just [arg NormalArg "AFoo" (Just $ typ "TBar") Nothing]) @=?) $
      parse typeArguments' "" "(AFoo: TBar)"
    , testCase "One 'default' argument" $
      (Right (Just [arg ConstArg "Default" (Just $ typ "TBar") Nothing]) @=?) $
      parse typeArguments' "" "(const Default: TBar)"
    , testCase "One argument with default" $
      (Right (Just [arg NormalArg "aFoo" (Just $ typ "TFoo") (Just (I $ Lexeme "" 32))]) @=?) $
      parse typeArguments' "" "(aFoo: TFoo = 32)"
    , testCase "One argument with default array" $
      (Right (Just [arg NormalArg "aFoo" (Just $ typ "TFoo") (Just $ L [I $ Lexeme "" 32, I $ Lexeme "" 33])]) @=?) $
      parse typeArguments' "" "(aFoo: TFoo = [32, 33])"
    , testCase "One const 'array of const' argument" $
      (Right (Just [arg ConstArg "Default" (Just $ DynamicArray 1 ConstType) Nothing]) @=?) $
      parse typeArguments' "" "(const Default: array of const)"
    , testCase "Two simple argument" $
      (Right
         (Just
            [ arg NormalArg "AFoo" (Just $ typ "TBar") Nothing
            , arg NormalArg "ABaz" (Just $ typ "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo: TBar; ABaz: TFuux)"
    , testCase "Two simple argument, with shorthand" $
      (Right
         (Just
            [ arg NormalArg "AFoo" (Just $ typ "TFuux") Nothing
            , arg NormalArg "ABaz" (Just $ typ "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo, ABaz: TFuux)"
    , testCase "Ensure that 'class' is a valid argument type" $
      (Right (Just [arg NormalArg "foo" (Just $ typ "class") Nothing]) @=?) $
      parse typeArguments' "" "(foo: class)"
    , testCase "Ensure that 'const' is a valid" $
      (Right (Just [arg ConstArg "foo" (Just $ typ "TFoo") Nothing]) @=?) $
      parse typeArguments' "" "(const foo: TFoo)"
    , testCase "Slightly complex example" $
      (Right
         (Just
            [ arg NormalArg "foo" (Just $ typ "TFoo") Nothing
            , arg NormalArg "bar" (Just $ typ "TFoo") Nothing
            , arg ConstArg "baz" (Just $ typ "TBaz.TBang") Nothing
            ]) @=?) $
      parse typeArguments' "" "(foo, bar: TFoo; const baz: TBaz.TBang)"
    ]
