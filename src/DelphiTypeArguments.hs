{-# LANGUAGE OverloadedStrings #-}

module DelphiTypeArguments (typeArguments) where

import Text.Megaparsec

import DelphiLexer
import DelphiAst

typeArguments :: Parser TypeName -> Parser ValueExpression -> Parser [Argument]
typeArguments typeName expression = concat <$> ( sepBy (do
  l <- (do
    c <- optional $ rword "const"
    v <- optional $ rword "var"
    o <- optional $ rword "out"
    let m = tmod c v o

    i <- identifier'
    return (m, i)
    ) `sepBy` symbol ","
  _ <- symbol ":"
  r <- typeName
  d <- optional $ do
    symbol "="
    expression

  return $ map (\(m, a) -> Arg m a r d) l
  ) semi )

  where
    tmod (Just _) _ _ = ConstArg
    tmod _ (Just _) _ = VarArg
    tmod _ _ (Just _) = OutArg
    tmod _ _ _ = NormalArg
