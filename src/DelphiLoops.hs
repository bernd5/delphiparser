{-# LANGUAGE OverloadedStrings #-}

module DelphiLoops (loop) where

import Prelude hiding (repeat)

import Text.Megaparsec
import Text.Megaparsec.Expr

import DelphiLexer
import DelphiAst

type ExpressionParser = Parser ValueExpression

loop :: Parser ValueExpression -> Parser Expression -> Parser Expression
loop a b = choice
  [ for a b
  , while a b
  , repeat a b]

for :: Parser ValueExpression -> Parser Expression -> Parser Expression
for expression statement = do
  rword "for"
  var <- identifier'
  symbol ":="
  from <- expression
  dir <-(LoopUpTo <$ rword "to") <|> (LoopDownTo <$ rword "downto")
  to <- expression
  rword "do"
  b <- statement
  return $ For (V var := from) dir to b

while :: Parser ValueExpression -> Parser Expression -> Parser Expression
while expression statement = do
  rword "while"
  cond <- expression
  rword "do"
  b <- statement
  return $ While cond b

repeat :: Parser ValueExpression -> Parser Expression -> Parser Expression
repeat expression statement = do
  rword "repeat"
  b <- statement `sepBy` semi
  rword "until"
  cond <- expression
  return $ Repeat b cond
