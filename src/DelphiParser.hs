{-# LANGUAGE OverloadedStrings #-}

module DelphiParser
  ( dUnitP
  , expression
  , uses
  , array'
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

property' :: Parser Field
property' = property typeName dArgumentP expression

typeArguments'' :: Char -> Char -> Parser (Maybe [Argument])
typeArguments'' a b  = optional (parens' a b $ typeArguments typeName expression)

typeArguments' :: Parser (Maybe [Argument])
typeArguments' = typeArguments'' '(' ')'

delphiTry' :: Parser Expression
delphiTry' = delphiTry statement

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
    symbol "="
    ie <- choice
      [ try $ dReferenceToProcedureP ident 
      , try $ dGenericRecordP ident args 
      , try $ dClassP ident args 
      , try $ setDefinition ident args 
      , try $ enumDefinition ident args  -- Contains parens
      , try $ typeAlias ident args
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

dGenericRecordP :: Text -> [Argument] -> Parser TypeDefinition
dGenericRecordP ident args = do
  rword "record"
  d <- many dFieldDefinitionP
  r <- dRecordDefinitionListP
  rword "end"
  return $ Record (GenericDefinition ident args) ((DefaultAccessibility d):r)

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
dFieldDefinitionP = do
  optional $ rword "class"
  dSimpleFieldP <|> dConstructorFieldP <|> dDestructorFieldP <|> dProcedureP <|> dFunctionP <|> property'

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
simplifyTypeName m (Just "^") (Just []) = AddressOfType $ Type m
simplifyTypeName m (Just "@") (Just []) = TargetOfPointer $ Type m
simplifyTypeName m (Nothing) (Just []) = Type m
simplifyTypeName m p (Just (x:xs)) = GenericInstance m (x : xs)
simplifyTypeName m p Nothing = Type m

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

statement :: Parser Expression
statement = choice
  [ try dEqExpression
  , try dIfExpression
  , try dBeginEndExpression
  , try dValueExpression
  , try loop'
  , delphiTry'
  , delphiCase'
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
    , Prefix (AddressOf <$ symbol "@")
    , binary (:<>) "<>"
    , binary (:+) "+"
    , binary (:-) "-"
    , binary (:==) "="
    , binary (:*) "*"
    , binary (:/) "/"
    , binary (:&) "and"
    , binary (:|) "or"
    , binary As "as"
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

functionImpl :: Parser ImplementationSpec
functionImpl = do
  rword "function"
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  typ <- symbol ":" *> typeName
  _ <- semi
  annotations <- many annotation
  optional interfaceItems
  statements <- dBeginEndExpression
  _ <- semi
  return $ FunctionImpl name args typ statements

procedureImpl :: Parser ImplementationSpec
procedureImpl = do
  rword "procedure"
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  _ <- semi
  annotations <- many annotation
  optional interfaceItems
  statements <- dBeginEndExpression
  _ <- semi
  return $ ProcedureImpl name args statements

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
  optional interfaceItems
  statements <- dBeginEndExpression
  _ <- semi
  return $ b name member args (fromMaybe UnspecifiedType typ) statements

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
annotation = choice
  [ (rword "override" *> semi) $> Override
  , (rword "virtual" *> semi) $> Virtual
  , (rword "default" *> semi) $> Default
  , (rword "stdcall" *> semi) $> StdCall
  ]

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

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword "implementation"
  uses <- optional uses
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
  return $ Implementation (Uses (fromMaybe [] uses)) functions

dUnitInitializationP :: Parser Initialization
dUnitInitializationP = do
  _ <- optional $ rword "initialization"
  return Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  _ <- optional $ rword "finalization"
  return Finalization
