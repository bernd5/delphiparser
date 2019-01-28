{-# LANGUAGE OverloadedStrings #-}

module TestSupport
 (testCase', ifDef, ifDef', include, c)

where

import Data.Text (unpack, Text)
import DelphiLexer (Parser)
import DelphiAst
import           Test.Tasty                     ( TestTree)
import           Test.Tasty.HUnit               ( testCase
                                                , (@=?)
                                                )
import           Text.Megaparsec                ( parse )
import Data.Either

testCase' :: (Eq e, Show e) => Text -> Parser e -> e -> TestTree
testCase' n p e = testCase (unpack n) $ (Right e @=?) $ parse p "" n

ifDef :: Text -> Text -> Text -> [Either Directive Text]
ifDef a "" c = [Left $ IfDef a [] [Right c]]
ifDef a b "" = [Left $ IfDef a [Right b] []]
ifDef a b c = [Left $ IfDef a [Right b] [Right c]]

ifDef' :: Text -> Text -> Text -> d -> Lexeme d
ifDef' a b c d = Lexeme (either id undefined $ head $ ifDef a b c) d

c :: Text -> Directive
c a = Comment a
include :: Text -> Directive
include a = Include a
