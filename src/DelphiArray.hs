{-# LANGUAGE OverloadedStrings #-}

module DelphiArray ( array, arrayDimension, arrayIndex ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

-- Many of these arrays are potentially recursive, hence,
-- they take a 'typeName' parser to parse the recursive types.
-- Ie, a type can consist of an array, of a type.

array :: Parser TypeName -> Parser ValueExpression -> Parser TypeName
array p e = rword "array" *> choice
    [ try $ variantArray p e
    , staticArray p e
    , dynamicArray p e
    , openDynamicArray p e]

arrayDimension :: Parser TypeName -> Parser ValueExpression -> Parser ArrayIndex
arrayDimension _ expression = try $ IndexOf <$> (expression `sepBy1` symbol ",")

arrayIndex :: Parser TypeName -> Parser ValueExpression -> Parser ArrayIndex
arrayIndex typeName expression = parens "[" "]" $ arrayDimension typeName expression

staticArray :: Parser TypeName -> Parser ValueExpression -> Parser TypeName
staticArray typeName expression = do
  index <- arrayIndex typeName expression
  rword "of"
  StaticArray index <$> typeName

dynamicArray :: Parser TypeName -> Parser ValueExpression -> Parser TypeName
dynamicArray typeName _ = do
  rword "of"
  dimensions <- fromIntegral . length <$> many (do
    rword "array"
    rword "of")
  base <- choice [ const ConstType <$> rword "const"
                 , typeName
                 ]
  return $ DynamicArray (dimensions+1) base

variantArray :: Parser TypeName -> Parser ValueExpression -> Parser TypeName
variantArray typeName expression = do
  index <- arrayIndex typeName expression
  rword "of"
  rword "const"
  return $ VariantArray index

openDynamicArray :: Parser TypeName -> Parser ValueExpression -> Parser TypeName
openDynamicArray typeName _ = OpenDynamicArray <$> typeName

