{-# LANGUAGE OverloadedStrings #-}

module DelphiTypeDefinition ( typeAttribute) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

typeAttribute:: Parser ValueExpression -> Parser TypeDefinition -> Parser TypeDefinition
typeAttribute expression typeDefinition= do
  symbol "["
  exprs <- try expression `sepBy` symbol ","
  symbol "]"
  typ <- try typeDefinition
  return $ TypeAttribute exprs typ
