{-# LANGUAGE OverloadedStrings #-}
module DelphiExpressions ( expression
  , lambdaExpression
  , lambdaFunction
  , lambdaProcedure
  , lambdaArgs
  , stringLiteral
  , functionCall) where

import Data.Text (Text, pack)
import Data.Char (chr)
import Data.Maybe (fromMaybe)
import DelphiAst
import DelphiLexer
import DelphiTypeArguments (typeArguments)
import DelphiArray (arrayIndex)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char (char, anyChar)

expression
  :: Parser Expression
  -> Parser InterfaceExpression
  -> Parser TypeName
  -> Parser ValueExpression
expression a b c = makeExprParser (termWithPrefixAndPostfix a b c) table

data Operator' m a =  Prefix' (m (a -> a)) | Postfix' (m (a -> a))

parseOperator :: Operator' Parser ValueExpression -> Parser (ValueExpression -> ValueExpression)
parseOperator (Prefix' m) = m
parseOperator (Postfix' m) = m

prefixes :: [Operator' Parser ValueExpression]
prefixes = [ prefix' Dereference  "^"
           , prefix' AddressOf  "@"
           , prefix' Not  "not"
           , prefix' Negate "-"
           ]

termWithPrefixAndPostfix :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
termWithPrefixAndPostfix a b c = do
  s <- termWithPrefixAndPostfix' a b c

  let postfixes = [ postfix' AddressOf  "@"
                  , postfix' Dereference  "^"
                  , Postfix' (functionCall a b c)
                  , Postfix' (indexCall a b c)
                  , Postfix' genericArgs
                  , Prefix' (do
                     _ <- try $ symbol "." <* notFollowedBy "."
                     term' <- terms a b c
                     return ( :. term'))
                  ]

  px <- many $ choice $ parseOperator <$> postfixes
  let px' = foldl (\n f -> f n) s px

  return $ px'

termWithPrefixAndPostfix' :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
termWithPrefixAndPostfix' a b c = do
  pr <- many $ choice $ parseOperator <$> prefixes
  s <- choice [ terms a b c ]
  return $ foldr (\f n -> f n) s pr

stringLiteral :: Parser ValueExpression
stringLiteral = do
  c <- comment
  strings <- some ((S  . (Lexeme c) . pack <$> (char '\'' >> manyTill anyChar (symbol "'")))
    <|> (symbol "#" *> (ToChar <$> I <$> integer)))
  return $ foldr f (S (Lexeme "" "")) strings
  where
    f :: ValueExpression -> ValueExpression -> ValueExpression
    f (S a) (S b) = S (a <> b)
    f (S a) (ToChar (I b)) = S (a <> c b)
    f (ToChar (I a)) (S b)= S (c a <> b)
    f (ToChar (I a)) (ToChar (I b))= S (c a <> c b)

    c :: Lexeme Integer -> Lexeme Text
    c (Lexeme cmt a) = Lexeme cmt (pack ( [chr ( fromIntegral a ) ]))

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
    , stringLiteral
    , try $ P <$> parens "(" ")" (expression a b c `sepBy1` symbol ",")
    , try $ L <$> parens "[" "]" (expression a b c `sepBy` symbol ",")
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

table
  :: [[Operator Parser ValueExpression]]
table = [ [ infixL (:*)  "*"
    , infixL (:/)  "/"
    , infixL (:%) "mod"
    , infixL (:/)  "div"
    ]
  , [ infixL (:<>) "<>"
    , infixL (:+) "+"
    , infixL (:-) "-"
    , infixL (:&) "and"
    , infixL (:+) "or"
    , infixL As  "as"
    , infixL Is  "is"
    , infixL In  "in"
    , infixL (:<=) "<="
    , infixL (:>=) ">="
    , infixL (:<) "<"
    , infixL (:>) ">"
    , infixL (:==) "="
    ]
  , [ infixL (:..) ".."
  ]
  ]
void :: a -> ()
void _ = ()


postfix' :: (ValueExpression -> ValueExpression)
             -> Text -> Operator' Parser ValueExpression
postfix' f name = Postfix' (f <$ symbol name)

prefix' :: (ValueExpression -> ValueExpression)
             -> Text -> Operator' Parser ValueExpression
prefix' f name = Prefix' (f <$ symbol name)

infixL :: (ValueExpression -> ValueExpression -> ValueExpression)
             -> Text -> Operator Parser ValueExpression
infixL f name = InfixL (f <$ symbol name)

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
functionCall a b c = flip (:$) <$> try (parens "(" ")" ((expression a b c) `sepBy` comma))

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
  args <- optional $ parens "(" ")" $ lambdaArgs beginEnd interfaceItems typeName
  let args' = fromMaybe [] args
  typ <- symbol ":" *> typeName
  nested <- many $ choice
    [ AdditionalInterface <$> try interfaceItems
    ]
  statements <- beginEnd
  return $ LambdaFunction args' typ nested statements

lambdaProcedure :: Parser Expression -> Parser InterfaceExpression -> Parser TypeName -> Parser ValueExpression
lambdaProcedure beginEnd interfaceItems typeName = do
  rword "procedure"
  args <- optional $ parens "(" ")" $ lambdaArgs beginEnd interfaceItems typeName
  let args' = fromMaybe [] args
  nested <- many $ choice
    [ AdditionalInterface <$> try interfaceItems
    ]
  statements <- beginEnd
  return $ LambdaProcedure args' nested statements
