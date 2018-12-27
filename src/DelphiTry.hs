{-# LANGUAGE OverloadedStrings #-}

module DelphiTry ( delphiTry ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

delphiTry :: Parser TypeName ->  Parser ValueExpression -> Parser Expression -> Parser Expression
delphiTry tn v e = choice
  [ try $ delphiTryFinally e
  , try $ delphiTryExcept e tn
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
  
  
delphiTryExcept :: Parser Expression -> Parser TypeName -> Parser Expression
delphiTryExcept statement typeName = do
  rword "try"
  b <- many $ statement <* semi
  rword "except"
  f <- choice [ try (concat <$> ( some $ try $ onExceptionHandlers typeName statement))
              , try $ crudeExceptionHandler statement]
  rword "end"
  return $ Try b (Left $ [ExceptOn Nothing f])

crudeExceptionHandler :: Parser Expression -> Parser [Expression]
crudeExceptionHandler statement = many $ statement <* semi

onExceptionHandlers :: Parser TypeName -> Parser Expression -> Parser [Expression]
onExceptionHandlers typeName statement = do
  rword "on"
  name <- optional (identifier <* symbol ":")
  typ <- typeName
  rword "do"
  statements <- many ( statement <* (optional semi))

  return $ statements
