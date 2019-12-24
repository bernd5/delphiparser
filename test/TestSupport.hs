{-# LANGUAGE OverloadedStrings #-}

module TestSupport
 (
 varDefinition,
 testCase',
 constDef,
 ifDef,
 ifDef',
 include,
 property,
 propertyRead,
 propertyWrite,
 genericInstance,
 c, typ, arg, v, s, i, field, lambdaFunction', lambdaArgs')

where

import Data.Text (unpack, Text)
import DelphiLexer (Parser)
import DelphiAst
import DelphiParser
import DelphiExpressions
import           Test.Tasty                     ( TestTree)
import           Test.Tasty.HUnit               ( testCase
                                                , (@=?)
                                                )
import           Text.Megaparsec                ( parse )

testCase' :: (Eq e, Show e) => Text -> Parser e -> e -> TestTree
testCase' n p e = testCase (unpack n) $ (Right e @=?) $ parse p "" n

ifDef :: Text -> Text -> Text -> [Either Directive Text]
ifDef a "" c' = [Left $ IfDef a [] [Right c']]
ifDef a b  "" = [Left $ IfDef a [Right b] []]
ifDef a b  c' = [Left $ IfDef a [Right b] [Right c']]

ifDef' :: Text -> Text -> Text -> d -> Lexeme d
ifDef' a b c' d = Lexeme (either id undefined $ head $ ifDef a b c') d

c :: Text -> Directive
c a = Comment a
include :: Text -> Directive
include a = Include a

typ :: Text -> TypeName
typ a = Type $ Lexeme NoDirective a

arg
  :: ArgModifier -> Text -> Maybe TypeName -> Maybe ValueExpression -> Argument
arg a b c' d = Arg a (Lexeme NoDirective b) c' d

v :: Text -> ValueExpression
v a = V $ Lexeme NoDirective a
s :: Text -> ValueExpression
s a = S $ Lexeme NoDirective a
i :: Integer -> ValueExpression
i a = I $ Lexeme NoDirective a
field :: Text -> TypeName -> Field
field a b = Field (Lexeme NoDirective a) b

lambdaFunction' :: Parser ValueExpression
lambdaFunction' = lambdaFunction beginEndExpression interfaceItems typeName

lambdaArgs' :: Parser [Argument]
lambdaArgs' = lambdaArgs beginEndExpression interfaceItems typeName

varDefinition a b c = VarDefinition (Lexeme NoDirective a) b c

property
  :: Text
  -> Maybe [Argument]
  -> TypeName
  -> Maybe ValueExpression
  -> [PropertySpecifier]
  -> Bool
  -> Field
property a b c' d e f = Property (Lexeme NoDirective a) b c' d e f
propertyRead :: [Text] -> PropertySpecifier
propertyRead a = PropertyRead (map (\x -> Lexeme NoDirective x) a)
propertyWrite :: [Text] -> PropertySpecifier
propertyWrite a = PropertyWrite (map (\x -> Lexeme NoDirective x) a)
genericInstance :: Text -> [TypeName] -> TypeName
genericInstance a b = GenericInstance (Lexeme NoDirective a) b

constDef :: Text -> Integer -> ConstDefinition
constDef a b = ConstDefinition (Lexeme NoDirective a) Nothing (I (Lexeme NoDirective b))
