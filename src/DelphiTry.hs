{-# LANGUAGE OverloadedStrings #-}

module DelphiTry ( delphiTry ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

delphiTry :: Parser ValueExpression -> Parser Expression -> Parser Expression
delphiTry v e = choice
  [ try $ delphiTryFinally e
  , try $ delphiTryExcept e
  , try $ delphiRaise v
  ]
delphiRaise :: Parser ValueExpression -> Parser Expression
delphiRaise expression = do
  rword "raise"
  b <- expression
  return $ Raise b

delphiTryFinally :: Parser Expression -> Parser Expression
delphiTryFinally statement = do
  rword "try"
  b <- many $ statement <* semi
  rword "finally"
  f <- many $ statement <* semi
  rword "end"
  return $ Try b (Right f)
  
  
delphiTryExcept :: Parser Expression -> Parser Expression
delphiTryExcept statement = do
  rword "try"
  b <- many $ statement <* semi
  rword "except"
  f <- many $ statement <* semi
  rword "end"
  return $ Try b (Left $ [ExceptOn Nothing f])
  
  
