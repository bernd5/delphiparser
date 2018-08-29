{-# LANGUAGE OverloadedStrings #-}

module DelphiArray ( array ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec
import Text.Megaparsec.Expr

-- Many of these arrays are potentially recursive, hence,
-- they take a 'typeName' parser to parse the recursive types.
-- Ie, a type can consist of an array, of a type.

array p = rword "array" *> choice
    [ try $ staticArray p
    , variantArray p
    , dynamicArray p
    , openDynamicArray p]

arrayDimension :: Parser TypeName -> Parser ArrayIndex
arrayDimension typeName = choice
  [ IndexOf <$> typeName
  , Range <$> sepBy (do
    lhs <- integer
    symbol ".."
    rhs <- integer
    return (lhs, rhs)) (symbol ",")]

arrayIndex :: Parser TypeName -> Parser ArrayIndex
arrayIndex typeName = parens "[" "]" $ arrayDimension typeName

staticArray :: Parser TypeName -> Parser TypeName
staticArray typeName = do
  index <- arrayIndex typeName
  rword "of"
  base <- typeName
  return $ StaticArray index base

dynamicArray :: Parser TypeName -> Parser TypeName
dynamicArray typeName = do
  rword "of"
  dimensions <- fromIntegral . length <$> many (do
    rword "array"
    rword "of")
  base <- typeName
  return $ DynamicArray (dimensions+1) base

variantArray :: Parser TypeName -> Parser TypeName
variantArray typeName = do
  index <- arrayIndex typeName
  rword "of"
  rword "const"
  return $ VariantArray index

openDynamicArray :: Parser TypeName -> Parser TypeName
openDynamicArray typeName  = OpenDynamicArray <$> typeName

