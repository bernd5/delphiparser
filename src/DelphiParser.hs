{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module DelphiParser
  ( unit
  , program
  , unitFragment
  , pascalFile
  , expression'
  , singleConstExpression
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
  , beginEndExpression
  , typeName
  , dValueExpression
  ) where

import Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.List (foldl')
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
expression' = expression beginEndExpression interfaceItems typeName

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

pascalFile :: Parser Unit
pascalFile = choice [unit, program, unitFragment]

unitFragment :: Parser Unit
unitFragment = try $ do
  c <- comment
  rst <- takeRest
  return $ UnitFragment c rst

unit :: Parser Unit
unit = try $ do
  _ <- optional $ char '\xFEFF'
  _ <- optional sc
  c <- comment
  rword "unit"
  unitName <- dottedIdentifier
  c' <- semi'
  interface <- dUnitInterfaceP
  implementation <- dUnitImplementationP
  initialization <- dUnitInitializationP
  finalization <- dUnitFinalizationP
  return $ Unit (mappend c c') unitName interface implementation initialization finalization

rword' :: Text -> (Directive -> Parser a) -> Parser a
rword' word f = do
  r <- rword word

  case r of
    Lexeme (Include a) () -> do
      let p = parse (f NoDirective) (unpack a) a
      case p of
        Left err -> fail $ show err
        Right result -> return result
    Lexeme (IfDef a b c) () -> f (IfDef a b c)
    Lexeme a () -> f a

program :: Parser Unit
program = try $ do
    _ <- optional $ char '\xFEFF'
    _ <- optional sc
    c1 <- comment
    rword "program"
    s <- anyIdentifier
    c2 <- semi'
    uses' <- (fromMaybe $ Uses [] NoDirective) <$> optional uses
    functions <-
      many $ choice 
        [ functionImpl
        , procedureImpl
        , dFunctionImplementationP
        , dProcedureImplementationP
        , dConstructorImplementationP
        , dDestructorImplementationP
        , rword "class" *> choice [
            dFunctionImplementationP
          , dProcedureImplementationP
          , dConstructorImplementationP
          , dDestructorImplementationP
          ]
        , AdditionalInterface <$> interfaceItems
        ]
    rword "begin"
    expressions <- many (statement <* semi)
    lastExpression <- optional statement
    rword "end."
    return $ Program (insertComment c1 s)
                     (insertComment' c2 uses')
                     functions
                     (expressions <> catMaybes [lastExpression])
  where
    insertComment' :: Directive -> Uses -> Uses
    insertComment' a (Uses b c) = Uses b (a <> c)


insertComment :: Directive -> Lexeme Text -> Lexeme Text
insertComment c (Lexeme d a) = Lexeme (c <> d) a

insertComment' :: Lexeme Text -> Directive-> Lexeme Text
insertComment' (Lexeme d a) c = Lexeme (d <> c) a

uses :: Parser Uses
uses = do
  rword "uses"
  items <- (do
              x <- comment
              y <- anyIdentifier
              return $ insertComment x y
            ) `sepBy` symbol' "." `sepBy` (char ',' <* sc)
  c <- semi'
  return $ Uses (concat items) c

dUnitInterfaceP :: Parser Interface
dUnitInterfaceP = do
  rword "interface"
  usings <- optional uses
  items <- many interfaceItems
  return $ Interface (fromMaybe (Uses [] NoDirective) usings) items

interfaceItems :: Parser InterfaceExpression
interfaceItems = choice
  [ typeExpressions
  , constExpressions 
  , resourceExpressions 
  , Standalone <$> dProcedureP
  , Standalone <$> dFunctionP
  , varExpressions
  ] 

singleConstExpression :: Parser ConstDefinition
singleConstExpression = do
  lhs <- anyIdentifier
  typ <- optional $ symbol ":" >> typeName
  c <- symbol' "="
  case c of
    Lexeme NoDirective "=" -> do
      rhs <- expression'
      optional semi
      return $ ConstDefinition lhs typ rhs
    Lexeme a "=" -> do
      return $ ConstDirectiveFragment lhs typ a

singleVarExpression :: Parser [VarDefinition]
singleVarExpression = do
  names <- anyIdentifier `sepBy` symbol ","
  typ <- symbol ":" >> typeName
  def <- optional (symbol "=" >> expression')
  semi
  return $ map (\x -> VarDefinition x typ def) names

constExpressions :: Parser InterfaceExpression
constExpressions = do
  rword "const"
  consts <- many singleConstExpression
  return $ ConstDefinitions consts

resourceExpressions :: Parser InterfaceExpression
resourceExpressions = do
  rword "resourcestring"
  consts <- many singleConstExpression
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
    args <- dGenericArgs
    let lhs' =
          if null args
            then Type ident
            else GenericDefinition ident args
    s <- symbol' "="
    case s of
      Lexeme NoDirective _ -> typeDefinitionRhs ident args lhs'
      Lexeme a "=" -> do
        return $ TypeDef lhs' (NewType $ Type (Lexeme a ""))

typeDefinitionRhs :: Lexeme Text -> [Argument] -> TypeName -> Parser TypeDefinition
typeDefinitionRhs a b c = do
    ie <- choice
      [ referenceTo *> choice [ dReferenceToProcedureP True a
                              , dReferenceToFunctionP True a
                              ]
      , dReferenceToProcedureP False a
      , dReferenceToFunctionP False a
      , dGenericRecordP c
      , interfaceType c stringLiteral
      , setDefinition c
      , enumDefinition c  -- Contains parens
      , do
        rword "class"
        r <- optional $ choice
          [ metaClassDefinition c
          , classHelper c
          , classType c
          ]
        return $ fromMaybe (ForwardClass c) r
      , newType c
      , typeAlias c -- Just for *very* simple type aliases
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
  rhs <- parens "(" ")" $ anyIdentifier`sepBy` symbol ","
  return $ EnumDefinition a rhs

referenceTo :: Parser ()
referenceTo = do
  rword "reference"
  rword "to"
  return ()

dReferenceToProcedureP :: Bool -> Lexeme Text -> Parser TypeDefinition
dReferenceToProcedureP isReference ident = do
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  o <- optional $ do
    rword "of"
    rword "object"
  let t = if isReference
          then ReferenceToProcedure
          else
            if isJust o
              then ProcedureOfObject
              else SimpleProcedure
  return $ TypeDef (Type ident) (t args)

dReferenceToFunctionP :: Bool -> Lexeme Text -> Parser TypeDefinition
dReferenceToFunctionP isReference ident = do
  rword "function"
  args <- dFunctionOrProcedureArgs'
  symbol ":"
  typ <- typeName
  o <- optional $ do
    rword "of"
    rword "object"
  isNested <- optional $ do
    rword "is"
    rword "nested"
  let t = if isReference
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
  parts <- anyIdentifier `sepBy` symbol "."
  return $ intercalateLexeme "." parts

intercalateLexeme :: Text -> [Lexeme Text] -> Lexeme Text
intercalateLexeme sep [] = Lexeme NoDirective ""
intercalateLexeme sep lexemes = foldr1 (\a b -> a <> (Lexeme NoDirective sep) <> b) lexemes

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
dFieldDefinitionP = do
  choice
    [ pure <$> recordCase
    , pure <$> dConstructorFieldP
    , pure <$> dDestructorFieldP
    , pure <$> dProcedureP
    , pure <$> dFunctionP
    , pure <$> property'
    , pure [] <* typeExpressions
    , rword "class" *> choice [classVar
                              , simpleField
                              , pure <$> dConstructorFieldP
                              , pure <$> dDestructorFieldP
                              , pure <$> dProcedureP
                              , pure <$> staticFunction
                              , pure <$> property'
                              ]
    , simpleField
    ] <* optional semi

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
    r a = UnspecifiedType' (pack $ "Unspecified pointer or reference type: " <> show a)

    t :: Maybe [TypeName] -> Maybe ArrayIndex -> (Lexeme Text -> TypeName)
    t x (Just y) = \a -> case a of
                          -- TODO: Unwrap a, and change it from an Array to a static array.
                          -- TODO: Or from a (Type (Lexeme "string")) to a static array of char.
                          _ -> StaticArray y ((t x Nothing) a)
    t (Just []) Nothing = Type
    t (Just (x:xs)) Nothing = flip GenericInstance (x:xs)
    t Nothing Nothing = Type

singleTypeName :: Parser TypeName
singleTypeName = choice [ array'
  , do
    -- TODO: Remind me again why I distinguish between singleTypeName, and typeName?
    -- TODO: Distinguish between the different sorts of identifiers, especially class.
    pointer <- optional $ (symbol' "^" <|> symbol' "@")
    name <-anyIdentifier
    args <- optional dGenericTypes
    ai <- optional $ arrayIndex typeName expression'
    return $ simplifyTypeName name pointer args ai
  ]

typeName :: Parser TypeName
typeName = do
  c <- comment
  case c of
    NoDirective -> typeName'
    Comment c -> typeName' >>= \x -> pure $ DirectiveType $ Lexeme (Comment c) x
    IfDef cond body els -> pure $ DirectiveType (Lexeme c UnspecifiedType)
    UnknownDirective _ -> pure $ DirectiveType $ Lexeme c UnspecifiedType
    els -> pure $ DirectiveType (Lexeme els UnspecifiedType)

typeName' :: Parser TypeName
typeName' = choice
        [ array'
        , setType
        , do
          -- TODO: Distinguish between the different sorts of identifiers, especially class.
          pointer <- optional $ (symbol' "^" <|> symbol' "@")
          name    <- anyIdentifier `sepBy1` symbol "."
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
  name <- anyIdentifier `sepBy1` symbol "."
  let name' = intercalateLexeme "." name
  eq <- optional $ ((symbol "=") *> anyIdentifier)
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
      generics <- dGenericArgs
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
  , dIfExpression
  , loop'
  , beginEndExpression
  , Break <$ rword "break"
  , dValueExpression
  , with'
  , delphiTry'
  , delphiCase'
  , const EmptyExpression <$> semi
  , Continue <$ rword "continue"
  ]

dValueExpression :: Parser Expression
dValueExpression = ExpressionValue <$> expression'

beginEndExpression :: Parser Expression
beginEndExpression = do
  rword "begin"
  comment
  expressions <- many (statement <* optional semi)
  end
  return $ Begin $ expressions

dEqExpression :: Parser Expression
dEqExpression = do
  lhs <- choice [ Result <$ rword "result"
                , V <$> dottedIdentifier
                ]
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
    [ procedureImpl
    , functionImpl
    , AdditionalInterface <$> interfaceItems
    ]
  statements <- beginEndExpression
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
    [ procedureImpl
    , functionImpl
    , AdditionalInterface <$> interfaceItems
    ]
  statements <- beginEndExpression
  _ <- semi
  return $ ProcedureImpl name args annotations nested statements

dMemberImplementationP ::
     Text
  -> (TypeName -> TypeName -> [Argument] -> TypeName -> [FieldAnnotation] -> [ImplementationSpec] -> Expression -> ImplementationSpec)
  -> Parser ImplementationSpec
dMemberImplementationP a b = do
  rword a
  name <- singleTypeName
  _ <- symbol "."
  member <- singleTypeName
  args <- dFunctionOrProcedureArgs'
  typ <- optional (symbol ":" *> typeName)
  _ <- semi
  annotations <- many annotation
  nested <- many $ choice
    [ AdditionalInterface <$> interfaceItems
    , procedureImpl
    , functionImpl
    ]
  statements <- beginEndExpression
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
  annotations <- many annotation
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
      m <- anyIdentifier
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
  name <- (Type <$> anyIdentifier) `sepBy1` symbol ","
  typ <- symbol ":" >> typeName
  semi
  return $ map (\x -> ClassVar x typ) name

dSimpleFieldP :: Parser [Field]
dSimpleFieldP = do
  sf <- simpleField
  optional semi
  return $ sf

simpleField :: Parser [Field]
simpleField = try $ do
  optional $ rword "const"
  comment
  name <- anyIdentifier `sepBy1` symbol ","
  symbol ":"
  typ <- typeName
  e <- optional $ symbol "="
  v <- if isJust e then optional $ expression'
                   else pure $ Nothing
  return $ map (\x -> Field x typ) name

dUnitImplementationP :: Parser Implementation
dUnitImplementationP = do
  rword' "implementation" $ \r -> do
    u <- optional uses
    functions <-
      many $ choice 
        [ functionImpl
        , procedureImpl
        , dFunctionImplementationP
        , dProcedureImplementationP
        , dConstructorImplementationP
        , dDestructorImplementationP
        , rword "class" *> choice [
            dFunctionImplementationP
          , dProcedureImplementationP
          , dConstructorImplementationP
          , dDestructorImplementationP
          ]
        , AdditionalInterface <$> interfaceItems
        ]
    return $ Implementation (fromMaybe (Uses [] NoDirective) u) functions

dUnitInitializationP :: Parser Initialization
dUnitInitializationP = do
  _ <- optional $ rword "initialization"
  return Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  _ <- optional $ rword "finalization"
  return Finalization
