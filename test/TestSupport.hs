{-# LANGUAGE OverloadedStrings #-}

module TestSupport
 (testCase', ifDef, ifDef', include, c, typ, arg, v, s, i, field)

where

import Data.Text (unpack, Text)
import DelphiLexer (Parser)
import DelphiAst
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
typ a = Type $ Lexeme Empty a

arg
  :: ArgModifier -> Text -> Maybe TypeName -> Maybe ValueExpression -> Argument
arg a b c' d = Arg a (Lexeme Empty b) c' d

v :: Text -> ValueExpression
v a = V $ Lexeme Empty a
s :: Text -> ValueExpression
s a = S $ Lexeme Empty a
i :: Integer -> ValueExpression
i a = I $ Lexeme Empty a
field :: Text -> TypeName-> Field
field a b = Field (Lexeme Empty a) b
