{-# LANGUAGE OverloadedStrings #-}

module DelphiProperty (property) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

import Data.Maybe (isJust)

-- http://docwiki.embarcadero.com/RADStudio/Berlin/en/Properties_(Delphi)

property :: Parser TypeName -> Parser [Argument] -> Parser ValueExpression -> Parser Field
property typeName arrayParameter expression = do
  rword "property"
  name <- identifier'
  indexes <- optional $ parens "[" "]" (arrayParameter `sepBy1` semi)
  symbol ":"
  typ <- typeName
  index <- optional $ do
    rword "index"
    index' <- expression
    return $ index'
  specifiers <- many $ specifier expression
  semi
  def <- optional $ do
    rword "default"
    semi
  return $ Property name (concat <$> indexes) typ index specifiers (isJust def)
  
specifier :: Parser ValueExpression -> Parser PropertySpecifier
specifier expression = do
  spec <- choice
    [ try $ PropertyRead <$> (rword "read" >> identifier')
    , try $ PropertyWrite <$> (rword "write" >> identifier')
    , try $ PropertyStored <$ rword "stored"
    , try $ PropertyDefault <$> (rword "default" >> expression)
    , try $ PropertyNoDefault <$ rword "nodefault"
    --, rword "implements"
    ]
  return spec
