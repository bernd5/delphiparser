{-# LANGUAGE OverloadedStrings #-}

module DelphiParser
  ( dUnitP
  , expression
  , dStatementP
  , dIfExpression
  , dFunctionImplementationP
  , dProcedureImplementationP
  , dTypeSpecListP
  , dValueExpression
  ) where

import Control.Monad (void)
import Data.Maybe
import Data.Text (Text, pack, strip)
import Data.Void
import DelphiAst
import DelphiLexer
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Data.Functor (($>))

dUnitP :: Parser Unit
dUnitP = do
  unitName <- dUnitNameP
  interface <- dUnitInterfaceP
  implementation <- dUnitImplementationP
  initialization <- dUnitInitializationP
  finalization <- dUnitFinalizationP
  return $ Unit unitName interface implementation initialization finalization

dUnitNameP :: Parser Text
dUnitNameP = do
  rword "unit"
  name <- strip . pack <$> identifier
  semi
  return name

dUnitInterfaceP :: Parser Interface
dUnitInterfaceP = do
  rword "interface"
  rword "type"
  types <- dTypeSpecListP
  return $ Interface types

dTypeSpecListP :: Parser [InterfaceExpression]
dTypeSpecListP =
  many $ do
    identifier <- pack <$> identifier
    args <- dGenericArgs
    symbol "="
    ie <-
      try (dReferenceToProcedureP identifier) <|>
      try (dGenericRecordP identifier args) <|>
      try (dClassP identifier args)
    semi
    return ie

dReferenceToProcedureP :: Text -> Parser InterfaceExpression
dReferenceToProcedureP identifier = do
  rword "reference"
  rword "to"
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  return $ TypeDef (Type identifier) (ReferenceToProcedure args)

dGenericRecordP :: Text -> [Argument] -> Parser InterfaceExpression
dGenericRecordP identifier args = do
  rword "record"
  r <- dRecordDefinitionListP
  rword "end"
  return $ Record (GenericDefinition identifier args) r

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = do
  symbol "("
  names <- ((Type . strip . pack) <$>) <$> sepBy identifier (symbol ",")
  symbol ")"
  return names

dClassP :: Text -> [Argument] -> Parser InterfaceExpression
dClassP identifier args = do
  rword "class"
  supers <- dArgsPassedP
  r <- dRecordDefinitionListP
  rword "end"
  return $
    if null supers
      then Class (GenericDefinition identifier args) supers r
      else Class (Type identifier) supers r

dArgumentListP :: Parser [Argument]
dArgumentListP = sepBy1 dArgumentP semi

dRecordDefinitionListP :: Parser [Accessibility]
dRecordDefinitionListP = many dRecordDefinitionP

dRecordDefinitionP :: Parser Accessibility
dRecordDefinitionP =
  dRecordDefinitionP' "public" Public <|>
  dRecordDefinitionP' "private" Private <|>
  dRecordDefinitionP' "protected" Protected

dRecordDefinitionP' ::
     String -> ([Field] -> Accessibility) -> Parser Accessibility
dRecordDefinitionP' a b = do
  rword a
  fields <- many dFieldDefinitionP
  return $ b fields

dFieldDefinitionP :: Parser Field
dFieldDefinitionP =
  dSimpleFieldP <|> dConstructorFieldP <|> dDestructorFieldP <|> dProcedureP <|>
  dFunctionP <|>
  property

dFunctionOrProcedureArgs :: String -> String -> Parser [Argument]
dFunctionOrProcedureArgs a b = do
  symbol a
  args <- sepBy1 dArgumentP semi
  symbol b
  return args

dFunctionOrProcedureArgs' :: Parser [Argument]
dFunctionOrProcedureArgs' =
  fromMaybe [] <$> optional (dFunctionOrProcedureArgs "(" ")")

dGenericArgs :: Parser [Argument]
dGenericArgs = fromMaybe [] <$> optional (dFunctionOrProcedureArgs "<" ">")

dGenericTypes :: Parser [TypeName]
dGenericTypes = many $ do
  symbol "<"
  -- TODO: This should be 'dTypeNameP', rather than identifier
  name <- strip . pack <$> identifier
  symbol ">"
  return $ Type name

dArrayOfP :: Parser TypeName
dArrayOfP = do
  rword "array"
  rword "of"
  typ <- dTypeNameP
  return $ Array typ

simplifyTypeName m (Just []) = Type m
simplifyTypeName m (Just (x:xs)) = GenericInstance m (x : xs)
simplifyTypeName m Nothing = Type m

dTypeNameP :: Parser TypeName
dTypeNameP =
  dArrayOfP <|> do
    name <- pack <$> anyIdentifier
    args <- optional dGenericTypes
    return $ simplifyTypeName name args

dFunctionP :: Parser Field
dFunctionP = do
  rword "function"
  name <- pack <$> identifier
  generics <- dGenericArgs
  let name' =
        if null generics
          then Type name
          else GenericDefinition name generics
  args <- dFunctionOrProcedureArgs'
  symbol ":"
  typ <- dTypeNameP
  semi
  return $ Function name' args typ []

dStatementP :: Parser Expression
dStatementP =
  try dEqExpression <|> try dIfExpression <|> try dBeginEndExpression <|>
  try dValueExpression <|>
  (const EmptyExpression <$> semi)

dValueExpression :: Parser Expression
dValueExpression = ExpressionValue <$> expression

dBeginEndExpression :: Parser Expression
dBeginEndExpression = do
  rword "begin"
  expressions <- many $ dStatementP <* semi
  rword "end"
  return $ Begin expressions

dEqExpression :: Parser Expression
dEqExpression = do
  lhs <- expression
  rword ":="
  rhs <- expression
  return $ lhs := rhs

expression :: Parser ValueExpression
expression = makeExprParser terms table

comma :: Parser Text
comma = pack <$> symbol ","

terms :: Parser ValueExpression
terms =
  choice
    [ V <$> identifier'
    , I <$> integer
    , Nil <$ rword "nil"
    , parens "(" ")" expression
    ]

-- Lots of inspiration from https://github.com/ilmoeuro/simplescript/blob/master/src/SimpleScript/Parser.hs
-- Thanks to liste on freenode for suggesting this.
manyPostfixOp :: Parser (a -> a) -> Parser (a -> a)
manyPostfixOp singleOp = foldr1 (flip (.)) <$> some singleOp

recordAccess = flip (:.) . V . pack <$> try (symbol "." *> identifier)

functionCall = flip (:$) <$> try (parens "(" ")" (expression `sepBy` comma))

indexCall = flip (:!!) <$> try (parens "[" "]" (expression `sepBy` comma))

genericArgs =
  flip (:<<>>) <$>
  try
    (do p <- parens "<" ">" ((Type <$> identifier') `sepBy1` comma)
        notFollowedBy $ choice [symbol "(", symbol "+", identifier]
        return p)

binary f name = InfixL (f <$ symbol name)

table :: [[Operator Parser ValueExpression]]
table =
  [ [ Postfix . manyPostfixOp $
      recordAccess <|> genericArgs <|> functionCall <|> indexCall
    , binary (:<>) "<>"
    , binary (:+) "+"
    , binary (:-) "-"
    , binary (:*) "*"
    , binary (:/) "/"
    , binary (:&) "and"
    , binary As "as"
    , binary (:<) "<"
    , binary (:>) ">"
    ]
  ]

dIfExpression :: Parser Expression
dIfExpression = do
  rword "if"
  expr <- expression
  rword "then"
  statement <- dStatementP
  elseStatement <- optional (rword "else" *> dStatementP)
  return $ If expr (Then statement)

dProcedureImplementationP =
  dMemberImplementationP "procedure" (\a b c d e -> MemberProcedureImpl a b c e)

dConstructorImplementationP =
  dMemberImplementationP
    "constructor"
    (\a b c d e -> MemberConstructorImpl a b c e)

dDestructorImplementationP =
  dMemberImplementationP "destructor" (\a b c d e -> MemberDestructorImpl a b e)

dFunctionImplementationP = dMemberImplementationP "function" MemberFunctionImpl

dMemberImplementationP ::
     String
  -> (TypeName -> TypeName -> [Argument] -> TypeName -> Expression -> ImplementationSpec)
  -> Parser ImplementationSpec
dMemberImplementationP a b = do
  rword a
  name <- dTypeNameP
  symbol "."
  member <- dTypeNameP
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> dTypeNameP)
  semi
  statements <- dBeginEndExpression
  semi
  return $ b name member args (fromMaybe UnspecifiedType typ) statements

property :: Parser Field
property = do
  rword "property"
  name <- pack <$> identifier
  args <- fromMaybe [] <$> optional (dFunctionOrProcedureArgs "[" "]")
  symbol ":"
  r <- dTypeNameP
  read <- (pack <$>) <$> optional (rword "read" *> identifier <* semi)
  write <- (pack <$>) <$> optional (rword "write" *> identifier <* semi)
  annotations <- many annotation
  return $ IndexProperty name (head args) r read write annotations

dProcedureP' ::
     String
  -> (Name -> [Argument] -> TypeName -> [Annotation] -> Field)
  -> Parser Field
dProcedureP' a b = do
  rword a
  name <- identifier
  args <- dFunctionOrProcedureArgs'
  semi
  annotations <- many annotation
  return $ b (pack name) args UnspecifiedType annotations

annotation :: Parser Annotation
annotation =
  ((rword "override" *> semi) $> Override) <|>
   ((rword "virtual" *> semi) $> Virtual) <|>
   ((rword "default" *> semi) $> Default)

dConstructorFieldP :: Parser Field
dConstructorFieldP = dProcedureP' "constructor" (\a b c d -> Constructor a b)

dDestructorFieldP :: Parser Field
dDestructorFieldP = dProcedureP' "destructor" (\a b c d -> Destructor a d)

dProcedureP :: Parser Field
dProcedureP = dProcedureP' "procedure" (\a b c d -> Procedure a b d)

dSimpleFieldP :: Parser Field
dSimpleFieldP = do
  name <- identifier
  symbol ":"
  typ <- dTypeNameP
  semi
  return $ Field (pack name) typ

dArgumentP :: Parser Argument
dArgumentP = do
  l <- identifier
  symbol ":"
  r <- dTypeNameP
  return $ Arg (pack l) r

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword "implementation"
  functions <-
    many
      (dFunctionImplementationP <|> dProcedureImplementationP <|>
       dConstructorImplementationP <|>
       dDestructorImplementationP)
  return $ Implementation functions

dUnitInitializationP :: Parser Initialization
dUnitInitializationP = do
  optional $ rword "initialization"
  return Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  optional $ rword "finalization"
  return Finalization
