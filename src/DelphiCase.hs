{-# LANGUAGE OverloadedStrings #-}

module DelphiCase (delphiCase) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

delphiCase :: Parser TypeName
           -> Parser Expression
           -> Parser ValueExpression
           -> Parser Expression
delphiCase typeName statement expression = do
  rword "case"
  c <- expression
  t <- optional (symbol ":" *> typeName)
  rword "of"
  items <- many $ do
    ordinal <- expression `sepBy` symbol ","
    symbol ":"
    s <- statement
    semi
    return $ CaseBranch ordinal s
  e <- optional $ do
    rword "else"
    s <- statement
    _ <- optional semi
    return $ Else s
  rword "end"

  return $ Case c  items e
  
