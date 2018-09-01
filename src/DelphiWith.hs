{-# LANGUAGE OverloadedStrings #-}

module DelphiWith ( with ) where

import DelphiLexer
import DelphiAst

with :: Parser ValueExpression -> Parser Expression -> Parser Expression
with expression statement = do
  rword "with"
  name <- expression
  rword "do"
  stmt <- statement
  return $ With name stmt
