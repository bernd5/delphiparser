{-# LANGUAGE OverloadedStrings #-}
module DelphiExpressions ( expression
  , functionCall) where

import Data.Text (Text, pack)
import DelphiAst
import DelphiLexer
import DelphiTypeArguments (typeArguments)
import DelphiArray (arrayIndex)

import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (char, anyChar)

expression
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> Parser ValueExpression
expression a b c = makeExprParser (terms a b c) (table a b c)

terms :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
terms a b c =
  choice
    [ V <$> identifierPlus ["string"]
    , F <$> float
    , I <$> integer
    , I <$> hexinteger
    , DTrue <$ rword "true"
    , lambdaExpression a b c
    , functionExit
    , DFalse <$ rword "false"
    , Inherited <$> (rword "inherited" >> optional identifier')
    , Result <$ rword "result"
    , Nil <$ rword "nil"
    , S . pack . concat <$> some ( char '\'' >> manyTill anyChar (symbol "'") )
    , try $  P <$> parens "(" ")" (expression a b c `sepBy1` symbol ",")
    , try recordExpression
    , parens "(" ")" (expression a b c)
    , A <$> arrayIndex c (expression a b c)
    ]
  where
    functionExit :: Parser ValueExpression
    functionExit = do
      rword "exit"
      e <- optional $ expression a b c
      return $ Exit e

    recordExpression :: Parser ValueExpression
    recordExpression = try $ RecordValue <$> parens "(" ")" (labelAndValue `sepBy` semi)

    labelAndValue :: Parser Expression
    labelAndValue = do
      lbl <- V <$> identifier'
      symbol ":"
      value <- expression a b c
      return $ lbl := value

-- Lots of inspiration from https://github.com/ilmoeuro/simplescript/blob/master/src/SimpleScript/Parser.hs
-- Thanks to liste on freenode for suggesting this.
manyPostfixOp :: Parser (a -> a) -> Parser (a -> a)
manyPostfixOp singleOp = foldr1 (flip (.)) <$> some singleOp


table
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> [[Operator Parser ValueExpression]]
table a b c =
  [ [ Postfix . manyPostfixOp $ choice
        [ try genericArgs
        , try $ functionCall a b c
        , try $ indexCall a b c 
        , try (Dereference <$ symbol "^")
        ]
    , Prefix (Not <$ rword "not")
    , Prefix (Dereference <$ symbol "^")
    , Prefix (AddressOf <$ symbol "@")
    , binary (:<>) "<>"
    , binary (:+) "+"
    , binary (:-) "-"
    , Prefix ((I 0 :-) <$ symbol "-")
    , binary (:==) "="
    , binary (:*) "*"
    , binary (:/) "/"
    , binary (:/) "div"
    , binary (:%) "mod"
    , binary (:&) "and"
    , binary (:..) ".."
    , binary (:.) "."
    , binary (:|) "or"
    , binary As "as"
    , binary Is "is"
    , binary In "in"
    , binary (:<=) "<="
    , binary (:>=) ">="
    , binary (:<) "<"
    , binary (:>) ">"
    ]
  ]

void :: a -> ()
void _ = ()

binary :: (ValueExpression -> ValueExpression -> ValueExpression)
             -> Text -> Operator Parser ValueExpression
binary f name = InfixL (f <$ symbol name)

genericArgs :: Parser (ValueExpression -> ValueExpression)
genericArgs =
  flip (:<<>>) <$>
  try
    (do p <- parens "<" ">" ((Type <$> identifier') `sepBy1` comma)
        notFollowedBy $ choice [void <$> identifier]
        return p)

functionCall
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> Parser (ValueExpression -> ValueExpression)
functionCall a b c =
  flip (:$) <$> try (parens "(" ")" ((expression a b c) `sepBy` comma))

indexCall :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser (ValueExpression -> ValueExpression)
indexCall a b c = flip (:!!) <$> try (parens "[" "]" ((expression a b c) `sepBy` comma))

comma :: Parser ()
comma = symbol ","

lambdaExpression
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> Parser ValueExpression
lambdaExpression a b c = choice [lambdaFunction a b c, lambdaProcedure a b c]


lambdaArgs
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> Parser [Argument]
lambdaArgs a b c = typeArguments c (expression a b c)

lambdaFunction :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
lambdaFunction beginEnd interfaceItems typeName = do
  rword "function"
  args <- lambdaArgs beginEnd interfaceItems typeName
  typ <- symbol ":" *> typeName
  nested <- many $ choice
    [ AdditionalInterface <$> try interfaceItems
    ]
  statements <- beginEnd
  return $ LambdaFunction args typ nested statements

lambdaProcedure :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
lambdaProcedure beginEnd interfaceItems typeName = do
  rword "procedure"
  args <- lambdaArgs beginEnd interfaceItems typeName
  nested <- many $ choice
    [ AdditionalInterface <$> try interfaceItems
    ]
  statements <- beginEnd
  return $ LambdaProcedure args nested statements
