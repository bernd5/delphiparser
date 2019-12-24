{-# LANGUAGE OverloadedStrings #-}

module DelphiTry ( delphiTry ) where

import DelphiLexer
import DelphiAst

import Text.Megaparsec

delphiTry :: Parser TypeName ->  Parser ValueExpression -> Parser Expression -> Parser Expression
delphiTry tn v e = choice
  [ tryFinallyExcept e tn
  , delphiRaise v
  ]
delphiRaise :: Parser ValueExpression -> Parser Expression
delphiRaise expression = do
  rword "raise"
  b <- expression
  return $ Raise b


tryFinallyExcept :: Parser Expression -> Parser TypeName -> Parser Expression
tryFinallyExcept statement typeName = do
  rword "try"
  b <- many $ statement <* semi
  choice [ tryFinally b statement
         , tryExcept b statement typeName
         ]


tryFinally :: [Expression] -> Parser Expression -> Parser Expression
tryFinally b statement = do
  rword "finally"
  f <- many $ statement <* semi
  rword "end"
  return $ Try b (Right f)


tryExcept :: [Expression] -> Parser Expression -> Parser TypeName -> Parser Expression
tryExcept b statement typeName = do
  rword "except"
  f <- choice [ (concat <$> ( some $ try $ onExceptionHandlers typeName statement))
              , crudeExceptionHandler statement]
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
