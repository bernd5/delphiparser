{-# LANGUAGE OverloadedStrings #-}

module DelphiTry ( delphiTry ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

delphiTry :: Parser Expression -> Parser Expression
delphiTry = delphiTryFinally

delphiTryFinally :: Parser Expression -> Parser Expression
delphiTryFinally statement = do
  rword "try"
  b <- many $ statement <* semi
  rword "finally"
  f <- many $ statement <* semi
  rword "end"
  return $ Try b (Right f)
  
  
