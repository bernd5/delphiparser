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

import Data.Maybe
import Data.Text (Text, pack, strip)
import DelphiAst
import DelphiLexer
import Text.Megaparsec
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
  _ <- semi
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
    ident <- pack <$> identifier
    args <- dGenericArgs
    symbol "="
    ie <-
      try (dReferenceToProcedureP ident) <|>
      try (dGenericRecordP ident args) <|>
      try (dClassP ident args)
    semi
    return ie

dReferenceToProcedureP :: Text -> Parser InterfaceExpression
dReferenceToProcedureP ident = do
  rword "reference"
  rword "to"
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  return $ TypeDef (Type ident) (ReferenceToProcedure args)

dGenericRecordP :: Text -> [Argument] -> Parser InterfaceExpression
dGenericRecordP ident args = do
  rword "record"
  r <- dRecordDefinitionListP
  rword "end"
  return $ Record (GenericDefinition ident args) r

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = do
  symbol "("
  names <- ((Type . strip . pack) <$>) <$> sepBy identifier (symbol ",")
  symbol ")"
  return names

dClassP :: Text -> [Argument] -> Parser InterfaceExpression
dClassP ident args = do
  rword "class"
  supers <- dArgsPassedP
  r <- dRecordDefinitionListP
  rword "end"
  return $
    if null supers
      then Class (GenericDefinition ident args) supers r
      else Class (Type ident) supers r

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
  symbol $ pack a
  args <- sepBy1 dArgumentP semi
  symbol $ pack b
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

simplifyTypeName :: Text -> Maybe [TypeName] -> TypeName
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

comma :: Parser ()
comma = symbol ","

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

recordAccess :: Parser (ValueExpression -> ValueExpression)
recordAccess = flip (:.) . V . pack <$> try (symbol "." *> identifier)

functionCall :: Parser (ValueExpression -> ValueExpression)
functionCall = flip (:$) <$> try (parens "(" ")" (expression `sepBy` comma))

indexCall :: Parser (ValueExpression -> ValueExpression)
indexCall = flip (:!!) <$> try (parens "[" "]" (expression `sepBy` comma))

void :: a -> ()
void _ = ()

genericArgs :: Parser (ValueExpression -> ValueExpression)
genericArgs =
  flip (:<<>>) <$>
  try
    (do p <- parens "<" ">" ((Type <$> identifier') `sepBy1` comma)
        notFollowedBy $ choice [symbol "(", symbol "+", void <$> identifier]
        return p)

binary :: (ValueExpression -> ValueExpression -> ValueExpression)
             -> Text -> Operator Parser ValueExpression
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
  return $ If expr (Then statement) (Else 
    (fromMaybe EmptyExpression elseStatement))
    

dProcedureImplementationP :: Parser ImplementationSpec
dProcedureImplementationP =
  dMemberImplementationP "procedure" (\a b c _ e -> MemberProcedureImpl a b c e)

dConstructorImplementationP :: Parser ImplementationSpec
dConstructorImplementationP =
  dMemberImplementationP
    "constructor"
    (\a b c _ e -> MemberConstructorImpl a b c e)

dDestructorImplementationP :: Parser ImplementationSpec
dDestructorImplementationP =
  dMemberImplementationP "destructor" (\a b _ _ e -> MemberDestructorImpl a b e)

dFunctionImplementationP :: Parser ImplementationSpec
dFunctionImplementationP = dMemberImplementationP "function" MemberFunctionImpl

dMemberImplementationP ::
     String
  -> (TypeName -> TypeName -> [Argument] -> TypeName -> Expression -> ImplementationSpec)
  -> Parser ImplementationSpec
dMemberImplementationP a b = do
  rword a
  name <- dTypeNameP
  _ <- symbol "."
  member <- dTypeNameP
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> dTypeNameP)
  _ <- semi
  statements <- dBeginEndExpression
  _ <- semi
  return $ b name member args (fromMaybe UnspecifiedType typ) statements

property :: Parser Field
property = do
  rword "property"
  name <- pack <$> identifier
  args <- fromMaybe [] <$> optional (dFunctionOrProcedureArgs "[" "]")
  _ <- symbol ":"
  r <- dTypeNameP
  rd <- (pack <$>) <$> optional (rword "read" *> identifier <* semi)
  wt <- (pack <$>) <$> optional (rword "write" *> identifier <* semi)
  annotations <- many annotation
  return $ IndexProperty name (head args) r rd wt annotations

dProcedureP' ::
     String
  -> (Name -> [Argument] -> TypeName -> [Annotation] -> Field)
  -> Parser Field
dProcedureP' a b = do
  rword a
  name <- identifier
  args <- dFunctionOrProcedureArgs'
  _ <- semi
  annotations <- many annotation
  return $ b (pack name) args UnspecifiedType annotations

annotation :: Parser Annotation
annotation =
  ((rword "override" *> semi) $> Override) <|>
   ((rword "virtual" *> semi) $> Virtual) <|>
   ((rword "default" *> semi) $> Default)

dConstructorFieldP :: Parser Field
dConstructorFieldP = dProcedureP' "constructor" (\a b _ _ -> Constructor a b)

dDestructorFieldP :: Parser Field
dDestructorFieldP = dProcedureP' "destructor" (\a _ _ d -> Destructor a d)

dProcedureP :: Parser Field
dProcedureP = dProcedureP' "procedure" (\a b _ d -> Procedure a b d)

dSimpleFieldP :: Parser Field
dSimpleFieldP = do
  name <- identifier
  _ <- symbol ":"
  typ <- dTypeNameP
  _ <- semi
  return $ Field (pack name) typ

dArgumentP :: Parser Argument
dArgumentP = do
  l <- identifier
  _ <- symbol ":"
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
  _ <- optional $ rword "initialization"
  return Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  _ <- optional $ rword "finalization"
  return Finalization
