{-# LANGUAGE OverloadedStrings #-}

module DelphiParser
  ( dUnitP
  , expression
  , uses
  , array'
  , with'
  , loop'
  , property'
  , typeArguments'
  , delphiCase'
  , functionCall
  , typeAlias
  , varExpressions
  , setDefinition
  , functionImpl
  , statement
  , delphiTry'
  , constExpressions
  , dIfExpression
  , dFieldDefinitionP
  , dFunctionP
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
import Data.Text (Text, pack, strip)
import DelphiAst
import DelphiLexer
import DelphiArray (array)
import DelphiLoops (loop)
import DelphiWith (with)
import DelphiProperty (property)
import DelphiTry (delphiTry)
import DelphiCase (delphiCase)
import DelphiTypeArguments (typeArguments)
import Text.Megaparsec
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))
import Text.Megaparsec.Char (char)

loop' :: Parser Expression
loop' = loop expression statement

with' :: Parser Expression
with' = with expression statement

property' :: Parser Field
property' = property typeName dArgumentP expression

typeArguments'' :: Char -> Char -> Parser (Maybe [Argument])
typeArguments'' a b  = optional (parens' a b $ typeArguments typeName expression)

typeArguments' :: Parser (Maybe [Argument])
typeArguments' = typeArguments'' '(' ')'

delphiTry' :: Parser Expression
delphiTry' = delphiTry expression statement

delphiCase' :: Parser Expression
delphiCase' = delphiCase statement expression

dArgumentP :: Parser [Argument]
dArgumentP = typeArguments typeName expression

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
  items <- many interfaceItems
  return $ Interface (Uses (fromMaybe [] usings)) items

interfaceItems :: Parser InterfaceExpression
interfaceItems = typeExpressions <|> constExpressions <|> varExpressions

singleConstExpression :: Parser ConstDefinition
singleConstExpression = do
  lhs <- identifier'
  typ <- optional $ symbol ":" >> array'
  symbol "="
  rhs <- choice [ parens "(" ")" (expression `sepBy` symbol ",")
                , (:[]) <$> expression]
  semi
  return $ ConstDefinition lhs typ rhs

singleVarExpression :: Parser VarDefinition
singleVarExpression = do
  lhs <- identifier'
  typ <- symbol ":" >> typeName
  semi
  return $ VarDefinition lhs typ

constExpressions :: Parser InterfaceExpression
constExpressions = do
  rword "const"
  consts <- many singleConstExpression
  return $ ConstDefinitions consts

varExpressions :: Parser InterfaceExpression
varExpressions = do
  rword "var"
  vars <- many singleVarExpression
  return $ VarDefinitions vars

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
    let lhs' =
          if null args
            then Type ident
            else GenericDefinition ident args
    symbol "="
    ie <- choice
      [ try $ dReferenceToProcedureP ident 
      , try $ dGenericRecordP lhs' 
      , try $ dClassP lhs' 
      , try $ setDefinition lhs' 
      , try $ enumDefinition lhs'  -- Contains parens
      , try $ typeAlias lhs'
      , forwardClass
      ]
    semi
    return ie

typeAlias :: TypeName ->  Parser TypeDefinition
typeAlias lhs = do
  rhs <- typeName
  return $ TypeAlias lhs rhs

setDefinition :: TypeName -> Parser TypeDefinition
setDefinition a = do
  rword "set"
  rword "of"
  rhs <- Type <$> identifier'
  return $ SetDefinition a rhs

enumDefinition :: TypeName -> Parser TypeDefinition
enumDefinition a = do
  rhs <- parens "(" ")" $ identifier' `sepBy` symbol ","
  return $ EnumDefinition a rhs

dReferenceToProcedureP :: Text -> Parser TypeDefinition
dReferenceToProcedureP ident = do
  r <- optional $ do
    rword "reference"
    rword "to"
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  o <- optional $ do
    rword "of"
    rword "object"
  let t = if isJust r
          then ReferenceToProcedure
          else
            if isJust o
              then ProcedureOfObject
              else SimpleProcedure
  return $ TypeDef (Type ident) (t args)

dGenericRecordP :: TypeName -> Parser TypeDefinition
dGenericRecordP a = do
  rword "record"
  d <- many dFieldDefinitionP
  r <- dRecordDefinitionListP
  rword "end"
  return $ Record a ((DefaultAccessibility d):r)

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = do
  symbol "("
  names <- ((Type . strip . pack) <$>) <$> sepBy identifier (symbol ",")
  symbol ")"
  return names

dClassP :: TypeName -> Parser TypeDefinition
dClassP a = do
  rword "class"
  supers <- dArgsPassedP
  r <- dRecordDefinitionListP
  rword "end"
  return $
    if null supers
      then Class a supers r
      else Class a supers r

dRecordDefinitionListP :: Parser [Accessibility]
dRecordDefinitionListP = many dRecordDefinitionP

dRecordDefinitionP :: Parser Accessibility
dRecordDefinitionP = choice
  [ dRecordDefinitionP' "public" Public
  , dRecordDefinitionP' "private" Private
  , dRecordDefinitionP' "protected" Protected
  , dRecordDefinitionP' "published" Published
  ]

dRecordDefinitionP' ::
     String -> ([Field] -> Accessibility) -> Parser Accessibility
dRecordDefinitionP' a b = do
  rword a
  fields <- many dFieldDefinitionP
  return $ b fields

dFieldDefinitionP :: Parser Field
dFieldDefinitionP = choice
  [ try dSimpleFieldP
  , try dConstructorFieldP
  , try dDestructorFieldP
  , try dProcedureP
  , try dFunctionP
  , try property'
  ]

dFunctionOrProcedureArgs' :: Parser [Argument]
dFunctionOrProcedureArgs' =
  fromMaybe [] <$> typeArguments'

dGenericArgs :: Parser [Argument]
dGenericArgs = fromMaybe [] <$> typeArguments'' '<' '>'

dGenericTypes :: Parser [TypeName]
dGenericTypes = many $ do
  symbol "<"
  -- TODO: This should be 'typeName', rather than identifier
  name <- strip . pack <$> identifier
  symbol ">"
  return $ Type name

array':: Parser TypeName
array' = array typeName expression

simplifyTypeName :: Text -> Maybe String -> Maybe [TypeName] -> TypeName
simplifyTypeName m a b = r a $ t b $ m
  where
    r (Just "^") = AddressOfType
    r (Just "@") = TargetOfPointer
    r Nothing = id
    r _ = error "Unspecified pointer or reference type"

    t (Just []) = Type
    t (Just (x:xs)) = flip GenericInstance (x:xs)
    t Nothing = Type

typeName :: Parser TypeName
typeName = choice [ try array',
  do
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    pointer <- optional $ (symbol' "^" <|> symbol' "@")
    name <- identifierPlus ["string", "boolean", "cardinal", "class"]
    args <- optional dGenericTypes
    return $ simplifyTypeName name pointer args
  ]

dFunctionP :: Parser Field
dFunctionP = do
  c <- optional $ rword "class"
  let c' = Static <$ c
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
  return $ Function name' args typ (catMaybes [c'])

statement :: Parser Expression
statement = choice
  [ try dEqExpression
  , try dIfExpression
  , try dBeginEndExpression
  , try dValueExpression
  , try loop'
  , try with'
  , try delphiTry'
  , try delphiCase'
  , const EmptyExpression <$> semi
  ]

dValueExpression :: Parser Expression
dValueExpression = ExpressionValue <$> expression

dBeginEndExpression :: Parser Expression
dBeginEndExpression = do
  rword "begin"
  expressions <- many $ statement <* semi
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
    , DTrue <$ rword "true"
    , DFalse <$ rword "false"
    , Inherited <$> (rword "inherited" >> identifier')
    , Result <$ rword "result"
    , Nil <$ rword "nil"
    , S . pack  <$> ( char '\'' >> manyTill L.charLiteral (char '\'') )
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
        notFollowedBy $ choice [void <$> identifier]
        return p)

binary :: (ValueExpression -> ValueExpression -> ValueExpression)
             -> Text -> Operator Parser ValueExpression
binary f name = InfixL (f <$ symbol name)

table :: [[Operator Parser ValueExpression]]
table =
  [ [ Postfix . manyPostfixOp $ choice
        [ try recordAccess
        , try genericArgs
        , try functionCall
        , try indexCall
        ]
    , Prefix (Not <$ rword "not")
    , Prefix (Dereference <$ symbol "^")
    , Postfix (Dereference <$ symbol "^")
    , Prefix (AddressOf <$ symbol "@")
    , binary (:<>) "<>"
    , binary (:+) "+"
    , binary (:-) "-"
    , Prefix ((I 0 :-) <$ symbol "-")
    , binary (:==) "="
    , binary (:*) "*"
    , binary (:/) "/"
    , binary (:&) "and"
    , binary (:|) "or"
    , binary As "as"
    , binary Is "is"
    , binary In "in"
    , binary (:<=) "<="
    , binary (:<) "<"
    , binary (:>) ">"
    ]
  ]

dIfExpression :: Parser Expression
dIfExpression = do
  rword "if"
  expr <- expression
  rword "then"
  s <- statement
  elseStatement <- optional (rword "else" *> statement)
  return $ If expr (Then s) (Else 
    (fromMaybe EmptyExpression elseStatement))
    

dProcedureImplementationP :: Parser ImplementationSpec
dProcedureImplementationP =
  dMemberImplementationP "procedure" (\a b c _ e f -> MemberProcedureImpl a b c e f)

dConstructorImplementationP :: Parser ImplementationSpec
dConstructorImplementationP =
  dMemberImplementationP
    "constructor"
    (\a b c _ e -> MemberConstructorImpl a b c e)

dDestructorImplementationP :: Parser ImplementationSpec
dDestructorImplementationP =
  dMemberImplementationP "destructor" (\a b _ _ e f -> MemberDestructorImpl a b e f)

dFunctionImplementationP :: Parser ImplementationSpec
dFunctionImplementationP = dMemberImplementationP "function" MemberFunctionImpl

functionImpl :: Parser ImplementationSpec
functionImpl = do
  rword "function"
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  typ <- symbol ":" *> typeName
  _ <- semi
  annotations <- many annotation
  nested <- many  ( AdditionalInterface <$> interfaceItems)
  statements <- dBeginEndExpression
  _ <- semi
  return $ FunctionImpl name args typ annotations nested statements

procedureImpl :: Parser ImplementationSpec
procedureImpl = do
  rword "procedure"
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  _ <- semi
  annotations <- many annotation
  nested <- many (AdditionalInterface <$> interfaceItems)
  statements <- dBeginEndExpression
  _ <- semi
  return $ ProcedureImpl name args annotations nested statements

dMemberImplementationP ::
     String
  -> (TypeName -> TypeName -> [Argument] -> TypeName -> [FieldAnnotation] -> [ImplementationSpec] -> Expression -> ImplementationSpec)
  -> Parser ImplementationSpec
dMemberImplementationP a b = do
  s <- optional $ (Static <$ rword "class")
  rword a
  name <- typeName
  _ <- symbol "."
  member <- typeName
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> typeName)
  _ <- semi
  annotations <- many annotation
  nested <- many (AdditionalInterface <$> interfaceItems)
  statements <- dBeginEndExpression
  _ <- semi
  return $ b name member args (fromMaybe UnspecifiedType typ) ((catMaybes [s]) <> annotations) nested statements

dProcedureP' ::
     String
  -> (TypeName -> [Argument] -> TypeName -> [FieldAnnotation] -> Field)
  -> Parser Field
dProcedureP' a b = do
  s <- optional $ (Static <$ rword "class")
  rword a
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  _ <- semi
  annotations <- many annotation
  return $ b name args UnspecifiedType ((catMaybes [s])<>annotations)

annotation :: Parser FieldAnnotation
annotation = choice
  [ (rword "override" *> semi) $> Override
  , (rword "virtual" *> semi) $> Virtual
  , (rword "default" *> semi) $> Default
  , (rword "stdcall" *> semi) $> StdCall
  ]

dConstructorFieldP :: Parser Field
dConstructorFieldP = dProcedureP' "constructor" (\a b _ d -> Constructor a b d)

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

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword "implementation"
  u <- optional uses
  functions <-
    many $ choice 
      [ try functionImpl
      , try procedureImpl
      , dFunctionImplementationP
      , dProcedureImplementationP
      , dConstructorImplementationP
      , dDestructorImplementationP
      , AdditionalInterface <$> interfaceItems
      ]
  return $ Implementation (Uses (fromMaybe [] u)) functions

dUnitInitializationP :: Parser Initialization
dUnitInitializationP = do
  _ <- optional $ rword "initialization"
  return Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  _ <- optional $ rword "finalization"
  return Finalization
