{-# LANGUAGE OverloadedStrings #-}

module DelphiTypeArguments (typeArguments, typeArgNames) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

import Data.Text (Text)

typeArgNames :: Parser [(ArgModifier, Text)]
typeArgNames = try $ (do
      c <- optional $ rword "const"
      v <- optional $ rword "var"
      o <- optional $ rword "out"
      let m = tmod c v o

      i <- identifierPlus ["default", "exclude"]
      return (m, i)
    ) `sepBy` symbol ","
  where
    tmod (Just _) _ _ = ConstArg
    tmod _ (Just _) _ = VarArg
    tmod _ _ (Just _) = OutArg
    tmod _ _ _ = NormalArg


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
