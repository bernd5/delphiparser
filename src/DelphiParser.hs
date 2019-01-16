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
  , newType
  , procedureImpl
  , typeDefinition
  , statement
  , delphiTry'
  , dEqExpression
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
import DelphiArray (array, arrayIndex)
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
  items <- (identifierPlus reserved) `sepBy` symbol "." `sepBy` symbol ","
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
  , Standalone <$> try dProcedureP
  , Standalone <$> try dFunctionP
  , try varExpressions
  ] 

singleConstExpression :: Parser ConstDefinition
singleConstExpression = do
  lhs <- identifier'
  typ <- optional $ symbol ":" >> typeName
  symbol "="
  rhs <- expression'
  optional semi
  return $ ConstDefinition lhs typ rhs

singleVarExpression :: Parser [VarDefinition]
singleVarExpression = do
  names <- (identifierPlus reserved) `sepBy` symbol ","
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
    s <- symbol' "="
    case s of
      Lexeme (Include (Lexeme e a)) b -> do
        --let c = parse (typeDefinitionRhs ident args lhs') "" a
        let c = Right $ TypeDef lhs' (NewType $ Type (Lexeme e ("todo-import:" <> a)))
        case c of
          Right c' -> return c'
          Left d' -> fail $ "Something failed"
      _ -> typeDefinitionRhs ident args lhs'

typeDefinitionRhs :: Lexeme Text -> [Argument] -> TypeName -> Parser TypeDefinition
typeDefinitionRhs a b c = do
    ie <- choice
      [ try $ dReferenceToProcedureP a
      , try $ dReferenceToFunctionP a
      , try $ dGenericRecordP c
      , try $ interfaceType c stringLiteral
      , try $ setDefinition c
      , try $ enumDefinition c  -- Contains parens
      , try $ do
        rword "class"
        r <- optional $ choice
          [ metaClassDefinition c
          , classHelper c
          , classType c
          ]
        return $ fromMaybe (ForwardClass c) r
      , try $ newType c
      , try $ typeAlias c -- Just for *very* simple type aliases
      , TypeExpression <$> expression'
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
  r <- optional $ dRecordDefinitionListP <* end
  let r' = fromMaybe [] r
  return $ TypeDef a $ ClassHelper b r'

setDefinition :: TypeName -> Parser TypeDefinition
setDefinition a = do
  rword "set"
  rword "of"
  rhs <- typeName
  return $ SetDefinition a rhs

setType :: Parser TypeName
setType = do
  rword "set"
  rword "of"
  rhs <- typeName
  return $ Set rhs

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
  optional dArgsPassedP
  r <- optional $ dRecordDefinitionListP <* end
  let r' = fromMaybe [] r
  c <- comment
  return $ if isNothing r
           then TypeAlias a (Type (Lexeme c "record"))
           else Record a r'

dottedIdentifier :: Parser (Lexeme Text)
dottedIdentifier = do
  parts <- (identifierPlus reserved) `sepBy` symbol "."
  return $ intercalateLexeme "." parts

intercalateLexeme :: Text -> [Lexeme Text] -> Lexeme Text
intercalateLexeme sep = foldr1 (\a b -> a <> (Lexeme Empty sep) <> b)

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = parens "(" ")" $ do
  -- names <- ((Type . strip . pack) <$>) <$> sepBy identifier (symbol ",")
  names <- sepBy dottedIdentifier (symbol ",")
  return $ map Type names


interfaceType :: TypeName -> Parser ValueExpression -> Parser TypeDefinition
interfaceType a guid = do
  rword "interface"
  supers <- optional dArgsPassedP
  optional $ parens "[" "]" guid
  let supers' = fromMaybe [] supers
  r <- optional $ dRecordDefinitionListP <* end
  let r' = fromMaybe [] r
  return $ InterfaceType a supers' r'

classType :: TypeName -> Parser TypeDefinition
classType a = do
  supers <- optional dArgsPassedP
  let supers' = fromMaybe [] supers
  r <- optional $ dRecordDefinitionListP <* end
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
     Text -> ([Field] -> Accessibility) -> Parser Accessibility
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
  , try $ pure [] <* typeExpressions
  , try $ rword "class" *> choice [classVar
                                  , try dSimpleFieldP
                                  , try $ pure <$> dConstructorFieldP
                                  , try $ pure <$> dDestructorFieldP
                                  , try $ pure <$> dProcedureP
                                  , try $ pure <$> staticFunction
                                  , try $ pure <$> property'
                                  ]
  , try dSimpleFieldP
  ] <* comment

recordCase :: Parser Field
recordCase = do
  rword "case"
  c <- expression'
  t <- optional (symbol ":" *> typeName)
  rword "of"
  items <- many (do
    notFollowedBy $ end
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

simplifyTypeName
  :: Lexeme Text
  -> Maybe (Lexeme Text)
  -> Maybe [TypeName]
  -> Maybe ArrayIndex
  -> TypeName
simplifyTypeName m a b c = r a $ t b c $ m
  where
    r :: Maybe (Lexeme Text) -> TypeName -> TypeName
    r (Just (Lexeme a "^")) = AddressOfType a
    r (Just (Lexeme a "@")) = TargetOfPointer a
    r Nothing = id
    r _ = error "Unspecified pointer or reference type"

    t :: Maybe [TypeName] -> Maybe ArrayIndex -> (Lexeme Text -> TypeName)
    t x (Just y) = \a -> case a of
                          -- TODO: Unwrap a, and change it from an Array to a static array.
                          -- TODO: Or from a (Type (Lexeme "string")) to a static array of char.
                          _ -> StaticArray y ((t x Nothing) a)
    t (Just []) Nothing = Type
    t (Just (x:xs)) Nothing = flip GenericInstance (x:xs)
    t Nothing Nothing = Type

singleTypeName :: Parser TypeName
singleTypeName = choice [ try array'
  , do
    -- TODO: Remind me again why I distinguish between singleTypeName, and typeName?
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    pointer <- optional $ (symbol' "^" <|> symbol' "@")
    name <- identifierPlus ["string", "boolean", "cardinal", "class"]
    args <- optional dGenericTypes
    ai <- optional $ arrayIndex typeName expression'
    return $ simplifyTypeName name pointer args ai
  ]

typeName :: Parser TypeName
typeName = do
  c <- comment
  case c of
    Include (Lexeme e a) -> do
        let c = Right $ Type (Lexeme e ("todo-import:" <> a))
        case c of
          Right c' -> return c'
          Left d' -> fail $ "Something failed"
    Comment a -> typeName'
    Empty -> typeName'

typeName' :: Parser TypeName
typeName' = choice
        [ try array'
        , try setType
        , do
          -- TODO: Distinguish between the different sorts of identifiers, especially class.
          pointer <- optional $ (symbol' "^" <|> symbol' "@")
          name    <- (identifierPlus reserved) `sepBy1` symbol "."
          let name' = intercalateLexeme "." name
          args <- optional dGenericTypes
          ai   <- optional $ arrayIndex typeName expression'
          return $ simplifyTypeName name' pointer args ai
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
  end
  return $ Begin $ expressions <> catMaybes [lastExpression]

dEqExpression :: Parser Expression
dEqExpression = do
  lhs <- V <$> dottedIdentifier
  symbol ":="
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
     Text
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
    Text 
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
  , (rword "noreturn" *> semi) $> NoReturn
  , (rword "inline" *> semi) $> Inline
  , (rword "final" *> semi) $> Final
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
  optional semi
  return $ sf

simpleField :: Parser [Field]
simpleField = do
  optional $ rword "const"
  comment
  name <- (identifierPlus reserved)`sepBy1` symbol ","
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
