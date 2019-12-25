{-# LANGUAGE OverloadedStrings #-}

module DelphiTypeArguments (typeArguments, typeArgNames) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

import Data.Text (Text)
import Data.Maybe (fromMaybe)

typeArgNames :: Parser [(ArgModifier, Lexeme Text)]
typeArgNames = try $ (do
      c <- optional $ choice [ ConstArg <$ rword "const"
                             , VarArg <$ rword "var"
                             , OutArg <$ rword "out"
                             ]
      let m = fromMaybe NormalArg c
      i <- anyIdentifier
      return (m, i)
    ) `sepBy` symbol ","


typeArguments :: Parser TypeName -> Parser ValueExpression -> Parser [Argument]
typeArguments typeName expression = concat <$> ( sepBy (do
  l <- typeArgNames
  r <- optional $ do
    _ <- symbol ":"
    typeName
  d <- optional $ do
    symbol "="
    expression

  return $ map (\(m, a) -> Arg m a r d) l
  ) semi )
