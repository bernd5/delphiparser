{-# LANGUAGE OverloadedStrings #-}

module DelphiWith ( with ) where

import DelphiLexer
import DelphiAst
import Text.Megaparsec (sepBy)

with :: Parser ValueExpression -> Parser Expression -> Parser Expression
with expression statement = do
  rword "with"
  name <- expression `sepBy` symbol ","
  rword "do"
  stmt <- statement
  return $ With name stmt
