{-# LANGUAGE OverloadedStrings #-}

module DelphiProperty (property) where

import Text.Megaparsec
import Text.Megaparsec.Expr

import DelphiLexer
import DelphiAst

import Data.Maybe (isJust)

import Data.Functor (($>))

-- http://docwiki.embarcadero.com/RADStudio/Berlin/en/Properties_(Delphi)

property :: Parser TypeName -> Parser [Argument] -> Parser Field
property typeName arrayParameter = do
  rword "property"
  name <- identifier'
  indexes <- optional $ parens "[" "]" (arrayParameter `sepBy1` semi)
  symbol ":"
  typ <- typeName
  index <- optional $ do
    rword "index"
    index' <- identifier'
    return $ index'
  specifiers <- many specifier
  semi
  def <- optional $ do
    rword "default"
    semi
  return $ Property name (concat <$> indexes) typ index specifiers (isJust def)
  
specifier :: Parser PropertySpecifier
specifier = do
  spec <- choice
    [ try $ PropertyRead <$> (rword "read" >> identifier')
    , try $ PropertyWrite <$> (rword "write" >> identifier')
    , try $ PropertyStored <$ rword "stored"
    , try $ PropertyDefault <$> (rword "default" >> identifierPlus ["True", "False"])
    , try $ PropertyNoDefault <$ rword "nodefault"
    --, rword "implements"
    ]
  return spec
