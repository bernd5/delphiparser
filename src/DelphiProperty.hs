{-# LANGUAGE OverloadedStrings #-}

module DelphiProperty (property) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

import Data.Text (Text)
import Data.Maybe (isJust)

-- http://docwiki.embarcadero.com/RADStudio/Berlin/en/Properties_(Delphi)

property :: Parser TypeName -> Parser [Argument] -> Parser ValueExpression -> Parser Field
property typeName arrayParameter expression = do
    rword "property"
    name <- identifier'
    rest name <|> inherited name
  where
    inherited :: Lexeme Text -> Parser Field
    inherited name = do
      specifiers <- many $ specifier expression
      semi
      return $ InheritedProperty name

    rest :: Lexeme Text -> Parser Field
    rest name = do
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

listOrExpression :: Parser ValueExpression -> Parser ValueExpression
listOrExpression expression = choice
  [ L <$> (parens "[" "]" (expression `sepBy` symbol ","))
  , expression
  ]
  
specifier :: Parser ValueExpression -> Parser PropertySpecifier
specifier expression = do
  spec <- choice
    [ try $ PropertyRead <$> (rword "read" >> (identifier' `sepBy` symbol "."))
    , try $ PropertyWrite <$> (rword "write" >> (identifier' `sepBy` symbol "."))
    , try $ PropertyStored <$ (rword "stored" >> optional expression)
    , try $ PropertyDefault <$> (rword "default" >> listOrExpression expression)
    , try $ PropertyNoDefault <$ rword "nodefault"
    --, rword "implements"
    ]
  return spec
