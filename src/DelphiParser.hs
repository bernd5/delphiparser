{-# LANGUAGE OverloadedStrings #-}

module DelphiParser
  ( dUnitP
  , program
  , expression'
  , typeExpressions
  , uses
  , array'
  , with'
  , classType
  , dArgsPassedP
  , dRecordDefinitionP
  , loop'
  , property'
  , typeArguments'
  , typeAttribute'
  , delphiCase'
  , functionCall
  , typeAlias
  , varExpressions
  , setDefinition
  , functionImpl
  , procedureImpl
  , typeDefinition
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
  , dConstructorImplementationP
  , singleVarExpression
  , dUnitImplementationP
  , interfaceItems
  , dBeginEndExpression
  , typeName
  , dValueExpression
  ) where

import Data.Maybe
import Data.Text (Text, strip, intercalate)
import DelphiAst
import DelphiLexer
import DelphiArray (array)
import DelphiLoops (loop)
import DelphiWith (with)
import DelphiProperty (property)
import DelphiTry (delphiTry)
import DelphiCase (delphiCase)
import DelphiTypeArguments (typeArguments)
import DelphiTypeDefinition (typeAttribute)
import DelphiExpressions (expression, functionCall, stringLiteral)
import Text.Megaparsec
import Data.Functor (($>))
import Text.Megaparsec.Char (char)

loop' :: Parser Expression
loop' = loop expression' statement

with' :: Parser Expression
with' = with expression' statement

expression' :: Parser ValueExpression
expression' = expression dBeginEndExpression interfaceItems typeName

property' :: Parser Field
property' = property typeName dArgumentP expression'

typeArguments'' :: Char -> Char -> Parser (Maybe [Argument])
typeArguments'' a b  = optional (parens' a b $ typeArguments typeName expression')

typeArguments' :: Parser (Maybe [Argument])
typeArguments' = typeArguments'' '(' ')'

typeAttribute' :: Parser TypeDefinition
typeAttribute' = typeAttribute expression' typeDefinition


delphiTry' :: Parser Expression
delphiTry' = delphiTry typeName expression' statement

delphiCase' :: Parser Expression
delphiCase' = delphiCase typeName statement expression'

dArgumentP :: Parser [Argument]
dArgumentP = typeArguments typeName expression'

dUnitP :: Parser Unit
dUnitP = try $ do
  _ <- optional $ char '\xFEFF'
  _ <- optional sc
  c <- comment
  unitName <- dUnitNameP
  interface <- dUnitInterfaceP
  implementation <- dUnitImplementationP
  initialization <- dUnitInitializationP
  finalization <- dUnitFinalizationP
  return $ Unit c unitName interface implementation initialization finalization

program :: Parser Unit
program = do
  _ <- optional $ char '\xFEFF'
  _ <- optional sc
  c <- comment
  rword "program"
  s <- identifier'
  semi
  optional uses
  functions <-
    many $ choice 
      [ try functionImpl
      , try procedureImpl
      , try dFunctionImplementationP
      , try dProcedureImplementationP
      , try dConstructorImplementationP
      , try dDestructorImplementationP
      , rword "class" *> choice [
          dFunctionImplementationP
        , dProcedureImplementationP
        , dConstructorImplementationP
        , dDestructorImplementationP
        ]
      , AdditionalInterface <$> interfaceItems
      ]
  rword "begin"
  expressions <- many (try $ statement <* semi)
  lastExpression <- optional statement
  rword "end."
  return $ Program s (expressions <> catMaybes [lastExpression])

dUnitNameP :: Parser (Lexeme Text)
dUnitNameP = rword "unit" *> dottedIdentifier <* semi

uses :: Parser [[Lexeme Text]]
uses = do
  rword "uses"
  items <- identifier' `sepBy` symbol "." `sepBy` symbol ","
  semi
  return items

dUnitInterfaceP :: Parser Interface
dUnitInterfaceP = do
  rword "interface"
  usings <- optional uses
  items <- try $ many interfaceItems
  return $ Interface (Uses (fromMaybe [] usings)) items

interfaceItems :: Parser InterfaceExpression
interfaceItems = choice
  [ try typeExpressions
  , try constExpressions 
  , try resourceExpressions 
  , try varExpressions
  , Standalone <$> try dProcedureP
  , Standalone <$> try dFunctionP
  ] 

singleConstExpression :: Parser ConstDefinition
singleConstExpression = do
  lhs <- identifier'
  typ <- optional $ symbol ":" >> typeName
  symbol "="
  rhs <- expression'
  semi
  return $ ConstDefinition lhs typ rhs

singleVarExpression :: Parser [VarDefinition]
singleVarExpression = do
  names <- identifier' `sepBy` symbol ","
  typ <- symbol ":" >> typeName
  def <- optional (symbol "=" >> expression')
  semi
  return $ map (\x -> VarDefinition x typ def) names

constExpressions :: Parser InterfaceExpression
constExpressions = do
  rword "const"
  consts <- many $ try singleConstExpression
  return $ ConstDefinitions consts

resourceExpressions :: Parser InterfaceExpression
resourceExpressions = do
  rword "resourcestring"
  consts <- many $ try singleConstExpression
  return $ ResourceDefinitions consts

varExpressions :: Parser InterfaceExpression
varExpressions = do
  rword "var"
  vars <- many $ try singleVarExpression
  return $ VarDefinitions $ concat vars

typeExpressions :: Parser InterfaceExpression
typeExpressions = do
  rword "type"
  types <- TypeDefinitions <$> many (choice [typeDefinition, typeAttribute'])
  return types


typeDefinition :: Parser TypeDefinition
typeDefinition = do
    ident <- identifier'
    args <- try dGenericArgs
    let lhs' =
          if null args
            then Type ident
            else GenericDefinition ident args
    symbol "="
    ie <- choice
      [ try $ dReferenceToProcedureP ident
      , try $ dReferenceToFunctionP ident
      , try $ dGenericRecordP lhs'
      , try $ interfaceType lhs' stringLiteral
      , try $ setDefinition lhs'
      , try $ enumDefinition lhs'  -- Contains parens
      , try $ do
        rword "class"
        r <- optional $ choice
          [ metaClassDefinition lhs'
          , classHelper lhs'
          , classType lhs'
          ]
        return $ fromMaybe (ForwardClass lhs') r
      , try $ typeAlias lhs' -- Just for *very* simple type aliases
      , try $ newType lhs'
      ]
    optional semi
    return ie

newType :: TypeName -> Parser TypeDefinition
newType lhs = do
  rword "type"
  rhs <- typeName
  return $ TypeDef lhs (NewType rhs)

typeAlias :: TypeName ->  Parser TypeDefinition
typeAlias lhs = do
  rhs <- typeName
  return $ TypeAlias lhs rhs

metaClassDefinition :: TypeName -> Parser TypeDefinition
metaClassDefinition a = do
  rword "of"
  rhs <- ClassOf <$> typeName
  return $ TypeDef a rhs

classHelper :: TypeName -> Parser TypeDefinition
classHelper a = do
  rword "helper"
  rword "for"
  b <- typeName
  r <- optional $ dRecordDefinitionListP <* rword "end"
  let r' = fromMaybe [] r
  return $ TypeDef a $ ClassHelper b r'

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

dReferenceToProcedureP :: Lexeme Text -> Parser TypeDefinition
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

dReferenceToFunctionP :: Lexeme Text -> Parser TypeDefinition
dReferenceToFunctionP ident = do
  r <- optional $ do
    rword "reference"
    rword "to"
  rword "function"
  args <- dFunctionOrProcedureArgs'
  symbol ":"
  typ <- typeName
  o <- optional $ do
    rword "of"
    rword "object"
  let t = if isJust r
          then ReferenceToFunction
          else
            if isJust o
              then FunctionOfObject
              else SimpleFunction
  return $ TypeDef (Type ident) (t args typ)

dGenericRecordP :: TypeName -> Parser TypeDefinition
dGenericRecordP a = do
  p <- optional $ rword "packed"
  choice [rword "record", rword "object"]
  r <- optional $ dRecordDefinitionListP <* rword "end"
  let r' = fromMaybe [] r
  c <- comment
  return $ if isNothing r
           then TypeAlias a (Type (Lexeme c "record"))
           else Record a r'

-- TODO: Fix this so that it's expressed in the type system.
dottedIdentifier :: Parser (Lexeme Text)
dottedIdentifier = do
  parts <- identifier' `sepBy` symbol "."
  return $ intercalateLexeme "." parts

intercalateLexeme :: Text -> [Lexeme Text] -> Lexeme Text
intercalateLexeme sep = foldr1 (\a b -> a <> (Lexeme "" sep) <> b) 

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = do
  symbol "("
  -- names <- ((Type . strip . pack) <$>) <$> sepBy identifier (symbol ",")
  names <- sepBy dottedIdentifier (symbol ",")
  symbol ")"
  return $ map Type names


interfaceType :: TypeName -> Parser ValueExpression -> Parser TypeDefinition
interfaceType a guid = do
  rword "interface"
  supers <- optional dArgsPassedP
  optional $ parens "[" "]" guid
  let supers' = fromMaybe [] supers
  r <- optional $ dRecordDefinitionListP <* rword "end"
  let r' = fromMaybe [] r
  return $ InterfaceType a supers' r'

classType :: TypeName -> Parser TypeDefinition
classType a = do
  supers <- optional dArgsPassedP
  let supers' = fromMaybe [] supers
  r <- optional $ dRecordDefinitionListP <* rword "end"
  let r' = fromMaybe [] r
  return $ if isNothing r
           then ForwardClass a
           else Class a supers' r'

dRecordDefinitionListP :: Parser [Accessibility]
dRecordDefinitionListP = do
  a <- concat <$> many dFieldDefinitionP
  b <- many (dRecordDefinitionP <* comment)
  return $ if null a
           then b
           else (DefaultAccessibility a) : b

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
  fields <- concat <$> many dFieldDefinitionP
  return $ b fields

dFieldDefinitionP :: Parser [Field]
dFieldDefinitionP = comment *> choice
  [ try $ pure <$> recordCase
  , try $ pure <$> dConstructorFieldP
  , try $ pure <$> dDestructorFieldP
  , try $ pure <$> dProcedureP
  , try $ pure <$> dFunctionP
  , try $ pure <$> property'
  , try dSimpleFieldP
  , try $ pure [] <* typeExpressions
  , try $ rword "class" *> choice [classVar
                                  , try dSimpleFieldP
                                  , try $ pure <$> dConstructorFieldP
                                  , try $ pure <$> dDestructorFieldP
                                  , try $ pure <$> dProcedureP
                                  , try $ pure <$> staticFunction
                                  , try $ pure <$> property'
                                  ]
  ] <* comment

recordCase :: Parser Field
recordCase = do
  rword "case"
  c <- expression'
  t <- optional (symbol ":" *> typeName)
  rword "of"
  items <- many (do
    notFollowedBy $ rword "end"
    ordinal <- expression' `sepBy1` symbol ","
    symbol ":"
    s <- parens "(" ")" (many (simpleField <* optional semi))
    optional semi
    return $ (ordinal, concat s))
  e <- optional $ do
    rword "else"
    s <- parens "(" ")" (simpleField `sepBy` semi)
    _ <- optional semi
    return $ concat s

  return $ CaseField c items e
  

dFunctionOrProcedureArgs' :: Parser [Argument]
dFunctionOrProcedureArgs' =
  fromMaybe [] <$> typeArguments'

dGenericArgs :: Parser [Argument]
dGenericArgs = fromMaybe [] <$> typeArguments'' '<' '>'

dGenericTypes :: Parser [TypeName]
dGenericTypes = do
  symbol "<"
  name <- typeName `sepBy1` symbol ","
  symbol ">"
  return $ name

array':: Parser TypeName
array' = array typeName expression'

simplifyTypeName :: Lexeme Text -> Maybe (Lexeme Text) -> Maybe [TypeName] -> TypeName
simplifyTypeName m a b = r a $ t b $ m
  where
    r :: Maybe (Lexeme Text) -> TypeName -> TypeName
    r (Just (Lexeme a "^")) = AddressOfType a
    r (Just (Lexeme a "@")) = TargetOfPointer a
    r Nothing = id
    r _ = error "Unspecified pointer or reference type"

    t (Just []) = Type
    t (Just (x:xs)) = flip GenericInstance (x:xs)
    t Nothing = Type

singleTypeName :: Parser TypeName
singleTypeName = choice [ try array'
  , do
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    pointer <- optional $ (symbol' "^" <|> symbol' "@")
    name <- identifierPlus ["string", "boolean", "cardinal", "class"]
    args <- optional dGenericTypes
    return $ simplifyTypeName name pointer args
  ]

typeName :: Parser TypeName
typeName = comment *> choice [ try array'
  , do
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    pointer <- optional $ (symbol' "^" <|> symbol' "@")
    name <- (identifierPlus reserved) `sepBy1` symbol "."
    let name' = intercalateLexeme "." name
    args <- optional dGenericTypes
    return $ simplifyTypeName name' pointer args
  ]

staticFunction :: Parser Field
staticFunction = a <$> dFunctionP
  where
    a (Function name args typ annotations) =
      Function name args typ (annotations <> [Static])
    a b = b

dFunctionP :: Parser Field
dFunctionP = do
  rword "function"
  name <- identifierPlus reserved `sepBy1` symbol "."
  let name' = intercalateLexeme "." name
  eq <- optional $ ((symbol "=") *> identifier')
  if isJust eq then do
    -- Is a redirect
    semi
    return $ RedirectedFunction name' (fromJust eq)
  else do
    -- Regular function
    desc name' <|> do
      semi
      return $ InheritedFunction name'
  where
    desc name = do
      generics <- try dGenericArgs
      let name' =
            if null generics
              then Type name
              else GenericDefinition name generics
      args <- dFunctionOrProcedureArgs'
      symbol ":"
      typ <- typeName
      semi
      annotations <- many annotation
      return $ Function name' args typ annotations

statement :: Parser Expression
statement = choice
  [ try dEqExpression
  , try dIfExpression
  , try loop'
  , try dBeginEndExpression
  , try $ Break <$ rword "break"
  , try dValueExpression
  , try with'
  , try delphiTry'
  , try delphiCase'
  , try $ const EmptyExpression <$> semi
  , Continue <$ rword "continue"
  ]

dValueExpression :: Parser Expression
dValueExpression = ExpressionValue <$> expression'

dBeginEndExpression :: Parser Expression
dBeginEndExpression = do
  rword "begin"
  comment
  expressions <- many (try $ statement <* semi)
  lastExpression <- optional statement
  rword "end"
  return $ Begin $ expressions <> catMaybes [lastExpression]

dEqExpression :: Parser Expression
dEqExpression = do
  lhs <- expression'
  rword ":=" -- TODO: Ensure that this doesn't require spaces.
  rhs <- expression'
  return $ lhs := rhs


dIfExpression :: Parser Expression
dIfExpression = do
  rword "if"
  expr <- expression'
  rword "then"
  s <- optional statement
  let s' = fromMaybe EmptyExpression s
  elseStatement <- optional (rword "else" *> statement)
  let elseStatement' = fromMaybe EmptyExpression elseStatement
  return $ If expr (Then s') (Else elseStatement')
    

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
  nested <- many $ choice
    [ try procedureImpl
    , try functionImpl
    , AdditionalInterface <$> try interfaceItems
    ]
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
  nested <- many $ choice
    [ try procedureImpl
    , try functionImpl
    , AdditionalInterface <$> try interfaceItems
    ]
  statements <- dBeginEndExpression
  _ <- semi
  return $ ProcedureImpl name args annotations nested statements

dMemberImplementationP ::
     String
  -> (TypeName -> TypeName -> [Argument] -> TypeName -> [FieldAnnotation] -> [ImplementationSpec] -> Expression -> ImplementationSpec)
  -> Parser ImplementationSpec
dMemberImplementationP a b = do
  comment
  rword a
  name <- singleTypeName
  _ <- symbol "."
  member <- singleTypeName
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> typeName)
  _ <- semi
  annotations <- many annotation
  nested <- many $ choice
    [ AdditionalInterface <$> try interfaceItems
    , try procedureImpl
    , try functionImpl
    ]
  statements <- dBeginEndExpression
  _ <- semi
  return $ b name member args (fromMaybe UnspecifiedType typ) annotations nested statements

dProcedureP' ::
     String
  -> (TypeName -> [Argument] -> TypeName -> [FieldAnnotation] -> Field)
  -> Parser Field
dProcedureP' a b = do
  rword a
  name <- typeName
  args <- dFunctionOrProcedureArgs'
  _ <- semi
  annotations <- many (try annotation)
  return $ b name args UnspecifiedType annotations

annotation :: Parser FieldAnnotation
annotation = choice
  [ (rword "override" *> semi) $> Override
  , (rword "overload" *> semi) $> Overload
  , (rword "reintroduce" *> semi) $> Reintroduce
  , (rword "virtual" *> semi) $> Virtual
  , (rword "default" *> semi) $> Default
  , (rword "dynamic" *> semi) $> Dynamic
  , (rword "abstract" *> semi) $> Abstract
  , (rword "stdcall" *> semi) $> StdCall
  , do
      rword "message"
      m <- identifier'
      semi
      return $ Message m
  ]

dConstructorFieldP :: Parser Field
dConstructorFieldP = dProcedureP' "constructor" (\a b _ d -> Constructor a b d)

dDestructorFieldP :: Parser Field
dDestructorFieldP = dProcedureP' "destructor" (\a _ _ d -> Destructor a d)

dProcedureP :: Parser Field
dProcedureP = dProcedureP' "procedure" (\a b _ d -> Procedure a b d)

classVar :: Parser [Field]
classVar = do
  rword "var"
  name <- (Type <$> identifier') `sepBy1` symbol ","
  typ <- symbol ":" >> typeName
  semi
  return $ map (\x -> ClassVar x typ) name

dSimpleFieldP :: Parser [Field]
dSimpleFieldP = do
  sf <- simpleField
  semi
  return $ sf

simpleField :: Parser [Field]
simpleField = do
  optional $ rword "const"
  comment
  name <- identifier' `sepBy1` symbol ","
  symbol ":"
  typ <- typeName
  e <- optional $ symbol "="
  v <- if isJust e then optional $ expression'
                   else pure $ Nothing
  return $ map (\x -> Field x typ) name

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword "implementation"
  u <- optional uses
  functions <-
    many $ choice 
      [ try functionImpl
      , try procedureImpl
      , try dFunctionImplementationP
      , try dProcedureImplementationP
      , try dConstructorImplementationP
      , try dDestructorImplementationP
      , rword "class" *> choice [
          dFunctionImplementationP
        , dProcedureImplementationP
        , dConstructorImplementationP
        , dDestructorImplementationP
        ]
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
