{-# LANGUAGE OverloadedStrings #-}

module TestTypeArguments
  ( typeArgumentTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import DelphiParser (typeArguments')
import Text.Megaparsec (parse)

import DelphiAst

typeArgumentTests :: TestTree
typeArgumentTests =
  testGroup
    "Delphi Type Argument Tests"
    [ testCase "Zero arguments, not even parens" $
      (Right Nothing @=?) $ parse typeArguments' "" ""
    , testCase "Zero arguments, but has parens" $
      (Right (Just []) @=?) $ parse typeArguments' "" "()"
    , testCase "One simple argument" $
      (Right (Just [Arg NormalArg "AFoo" (Type "TBar") Nothing]) @=?) $
      parse typeArguments' "" "(AFoo: TBar)"
    , testCase "Two simple argument" $
      (Right
         (Just
            [ Arg NormalArg "AFoo" (Type "TBar") Nothing
            , Arg NormalArg "ABaz" (Type "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo: TBar; ABaz: TFuux)"
    , testCase "Two simple argument, with shorthand" $
      (Right
         (Just
            [ Arg NormalArg "AFoo" (Type "TFuux") Nothing
            , Arg NormalArg "ABaz" (Type "TFuux") Nothing
            ]) @=?) $
      parse typeArguments' "" "(AFoo, ABaz: TFuux)"
    , testCase "Ensure that 'class' is a valid argument type" $
      (Right (Just [Arg NormalArg "foo" (Type "class") Nothing]) @=?) $
      parse typeArguments' "" "(foo: class)"
    , testCase "Ensure that 'const' is a valid" $
      (Right (Just [Arg ConstArg "foo" (Type "TFoo") Nothing]) @=?) $
      parse typeArguments' "" "(const foo: TFoo)"
    , testCase "Slightly complex example" $
      (Right
         (Just
            [ Arg NormalArg "foo" (Type "TFoo") Nothing
            , Arg NormalArg "bar" (Type "TFoo") Nothing
            , Arg ConstArg "baz" (Type "TBaz") Nothing
            ]) @=?) $
      parse typeArguments' "" "(foo, bar: TFoo; const baz: TBaz)"
    ]
