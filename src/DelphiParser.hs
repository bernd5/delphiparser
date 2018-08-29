{-# LANGUAGE OverloadedStrings #-}

module DelphiParser
  ( dUnitP
  , expression
  , uses
  , array'
  , typeAlias
  , setDefinition
  , dStatementP
  , dArgumentP
  , constExpressions
  , dIfExpression
  , dUnitInterfaceP
  , dFunctionImplementationP
  , dProcedureImplementationP
  , dFunctionOrProcedureArgs'
  , dUnitImplementationP
  , dTypeSpecListP
  , typeName
  , dValueExpression
  ) where

import Data.Maybe
import Safe (headMay)
import Data.Text (Text, pack, strip)
import DelphiAst
import DelphiLexer
import DelphiArray (array)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Data.Functor (($>))
import Text.Megaparsec.Char (char)

dUnitP :: Parser Unit
dUnitP = do
  _ <- optional $ char '\xFEFF'
  _ <- optional sc
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

uses :: Parser [Text]
uses = do
  rword "uses"
  items <- identifier' `sepBy` (symbol ",")
  semi
  return items

dUnitInterfaceP :: Parser Interface
dUnitInterfaceP = do
  rword "interface"
  usings <- optional uses
  items <- many (typeExpressions <|> constExpressions)
  return $ Interface (Uses (fromMaybe [] usings)) items

singleConstExpression :: Parser ConstDefinition
singleConstExpression = do
  lhs <- identifier'
  optional $ symbol ":" >> array'
  symbol "="
  rhs <- choice [ parens "(" ")" (expression `sepBy` symbol ",")
                , (:[]) <$> expression]
  semi
  return $ ConstDefinition lhs rhs

constExpressions :: Parser InterfaceExpression
constExpressions = do
  rword "const"
  consts <- many singleConstExpression
  return $ ConstDefinitions consts

typeExpressions :: Parser InterfaceExpression
typeExpressions = do
  rword "type"
  types <- TypeDefinitions <$> many dTypeSpecListP
  return types

forwardClass :: Parser TypeDefinition
forwardClass = do
  rword "class"
  return ForwardClass

dTypeSpecListP :: Parser TypeDefinition
dTypeSpecListP = do
    ident <- pack <$> identifier
    args <- dGenericArgs
    symbol "="
    ie <- choice
      [ dReferenceToProcedureP ident 
      , dGenericRecordP ident args 
      , try $ dClassP ident args 
      , setDefinition ident args 
      , enumDefinition ident args  -- Contains parens
      , typeAlias ident args
      , forwardClass
      ]
    semi
    return ie

typeAlias :: Text -> [Argument] ->  Parser TypeDefinition
typeAlias lhs args = do
  rhs <- typeName
  return $ TypeAlias (GenericDefinition lhs args) rhs

setDefinition :: Text -> [Argument] -> Parser TypeDefinition
setDefinition a b = do
  rword "set"
  rword "of"
  rhs <- Type <$> identifier'
  return $ SetDefinition (GenericDefinition a b) rhs

enumDefinition :: Text -> [Argument] -> Parser TypeDefinition
enumDefinition a b = do
  rhs <- parens "(" ")" $ identifier' `sepBy` symbol ","
  return $ EnumDefinition (GenericDefinition a b) rhs

dReferenceToProcedureP :: Text -> Parser TypeDefinition
dReferenceToProcedureP ident = do
  optional $ do
    rword "reference"
    rword "to"
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  optional $ do
    rword "of"
    rword "object"
  return $ TypeDef (Type ident) (ReferenceToProcedure args)

dGenericRecordP :: Text -> [Argument] -> Parser TypeDefinition
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

dClassP :: Text -> [Argument] -> Parser TypeDefinition
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
  dRecordDefinitionP' "protected" Protected <|>
  dRecordDefinitionP' "published" Published

dRecordDefinitionP' ::
     String -> ([Field] -> Accessibility) -> Parser Accessibility
dRecordDefinitionP' a b = do
  rword a
  fields <- many dFieldDefinitionP
  return $ b fields

dFieldDefinitionP :: Parser Field
dFieldDefinitionP = do
  optional $ rword "class"
  dSimpleFieldP <|> dConstructorFieldP <|> dDestructorFieldP <|> dProcedureP <|> dFunctionP <|> property

dFunctionOrProcedureArgs :: String -> String -> Parser [Argument]
dFunctionOrProcedureArgs a b = do
  symbol $ pack a
  args <- concat <$> sepBy1 dArgumentP semi
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
  -- TODO: This should be 'typeName', rather than identifier
  name <- strip . pack <$> identifier
  symbol ">"
  return $ Type name

array':: Parser TypeName
array' = array typeName

simplifyTypeName :: Text -> Maybe [TypeName] -> TypeName
simplifyTypeName m (Just []) = Type m
simplifyTypeName m (Just (x:xs)) = GenericInstance m (x : xs)
simplifyTypeName m Nothing = Type m

typeName :: Parser TypeName
typeName = choice [ array',
  do
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    name <- identifierPlus ["string", "boolean", "cardinal", "class"]
    args <- optional dGenericTypes
    return $ simplifyTypeName name args
  ]

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
  typ <- typeName
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
    , I <$> hexinteger
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
  name <- typeName
  _ <- symbol "."
  member <- typeName
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> typeName)
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
  r <- typeName
  id <- (pack <$>) <$> optional (rword "index" *> identifier)
  rd <- (pack <$>) <$> optional (rword "read" *> identifier)
  wt <- (pack <$>) <$> optional (rword "write" *> identifier)
  def <- optional (rword "default" *> expression)
  semi
  annotations <- many annotation
  return $ IndexProperty name (headMay args) r id rd wt def annotations

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
  typ <- typeName
  _ <- semi
  return $ Field (pack name) typ

dArgumentP :: Parser [Argument]
dArgumentP = do
  optional $ rword "const"
  optional $ rword "var"
  optional $ rword "out"
  l <- identifier' `sepBy` symbol ","
  _ <- symbol ":"
  r <- typeName
  optional $ do
    symbol "="
    anyIdentifier
  return $ map (\a -> Arg a r) l

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword "implementation"
  optional uses
  optional $ many (typeExpressions <|> constExpressions)
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
