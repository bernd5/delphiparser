{-# LANGUAGE OverloadedStrings #-}

module DelphiTypeDefinition ( typeAttribute) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

typeAttribute:: Parser ValueExpression -> Parser TypeDefinition -> Parser TypeDefinition
typeAttribute expression typeDefinition= do
  symbol "["
  exprs <- expression `sepBy` symbol ","
  symbol "]"
  typ <- typeDefinition
  return $ TypeAttribute exprs typ
