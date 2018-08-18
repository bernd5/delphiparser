{-# LANGUAGE OverloadedStrings #-}

module DelphiParser (
  parseDelphiUnit
  , dUnitP
  , dValueP
  , dStatementP
  , dIndexArgs
  , dIfExpression
  , dFunctionImplementationP
  , dProcedureImplementationP
)where

import DelphiAst
import DelphiLexer
import Control.Monad (void)
import Data.Void
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack)

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
  name <- identifier
  semi
  return $ pack name

dUnitInterfaceP :: Parser Interface
dUnitInterfaceP = do
  rword "interface"
  rword "type"
  types <- dTypeSpecListP
  return $ Interface types

dTypeSpecListP :: Parser [InterfaceExpression]
dTypeSpecListP = many $ (
         try dReferenceToProcedureP
         <|> try dGenericRecordP
         <|> dClassP) <* semi

dReferenceToProcedureP :: Parser InterfaceExpression
dReferenceToProcedureP = do
  identifier <- identifier
  symbol "="
  rword "reference"
  rword "to"
  rword "procedure"
  args <- dFunctionOrProcedureArgs'
  return $ TypeDef (Type $ pack identifier) (ReferenceToProcedure args)

dGenericRecordP :: Parser InterfaceExpression
dGenericRecordP = do
  identifier <- identifier
  args <- dGenericArgs
  symbol "="
  rword "record"
  r <- dRecordDefinitionListP
  return $ Record (GenericDefinition (pack identifier) args) r

dArgsPassedP :: Parser [TypeName]
dArgsPassedP = do
  symbol "("
  names <- ( ( Type . pack ) <$> ) <$> sepBy identifier (symbol ",")
  symbol ")"
  return $ names

dClassP :: Parser InterfaceExpression
dClassP= do
  identifier <- identifier
  args <- dGenericArgs
  symbol "="
  rword "class"
  supers <- dArgsPassedP
  r <- dRecordDefinitionListP
  return $ Record (GenericDefinition (pack identifier) args) r

dArgumentListP :: Parser [Argument]
dArgumentListP = sepBy1 dArgumentP semi

dRecordDefinitionListP :: Parser [Accessibility]
dRecordDefinitionListP = many dRecordDefinitionP <* (rword "end")

dRecordDefinitionP :: Parser Accessibility
dRecordDefinitionP = (dRecordDefinitionP' "public" Public)
                   <|> (dRecordDefinitionP' "private" Private)
                   <|> (dRecordDefinitionP' "protected" Protected)

dRecordDefinitionP' :: String -> ([Field] -> Accessibility) ->  Parser Accessibility
dRecordDefinitionP' a b = do
  rword a
  fields <- many $ dFieldDefinitionP
  return $ b fields

dFieldDefinitionP :: Parser Field
dFieldDefinitionP = dSimpleFieldP
                  <|> dConstructorFieldP
                  <|> dDestructorFieldP
                  <|> dProcedureP
                  <|> dFunctionP
                  <|> dPropertyP

dFunctionOrProcedureArgs :: String -> String -> Parser [Argument]
dFunctionOrProcedureArgs a b = do
  symbol a
  args <- sepBy1 dArgumentP semi
  symbol b
  return args

dFunctionOrProcedureArgs' :: Parser [Argument]
dFunctionOrProcedureArgs' = (fromMaybe []) <$> optional (
  dFunctionOrProcedureArgs "(" ")")

dGenericArgs :: Parser [Argument]
dGenericArgs = (fromMaybe []) <$> optional (
  dFunctionOrProcedureArgs "<" ">")

dGenericTypes :: Parser [TypeName]
dGenericTypes = ( many $ do
  symbol "<"
  -- TODO: This should be 'dTypeNameP', rather than identifier
  name <- pack <$> identifier
  symbol ">"
  return $ Type name)

dArrayOfP :: Parser TypeName
dArrayOfP = do
  rword "array"
  rword "of"
  dTypeNameP

simplifyTypeName m (Just []) = Type m
simplifyTypeName m (Just (x:xs)) = GenericInstance m (x:xs)
simplifyTypeName m Nothing = Type m

dTypeNameP :: Parser TypeName
dTypeNameP = dArrayOfP <|> do
  name <- pack <$> anyIdentifier
  args <- optional dGenericTypes
  return $ simplifyTypeName name args

dFunctionP :: Parser Field
dFunctionP = do
  rword "function"
  name <- identifier
  generics <- dGenericArgs
  args <- dFunctionOrProcedureArgs'
  symbol ":"
  typ <- dTypeNameP
  semi
  return $ Function (pack name) args typ []

dStatementP :: Parser Expression
dStatementP = (
            try dEqExpression
            <|> try dIfExpression
            <|> try dBeginEndExpression
            <|> try dValueExpression
            <|> ((\_ -> EmptyExpression) <$> semi)
            )

dValueExpression :: Parser Expression
dValueExpression = ExpressionValue <$> dValueP

dBeginEndExpression :: Parser Expression
dBeginEndExpression = do
  rword "begin"
  expressions <- many $ dStatementP <* semi
  rword "end"
  return $ Begin expressions

dEqExpression :: Parser Expression
dEqExpression = do
  lhs <- dValueP
  rword ":="
  rhs <- dValueP
  return $ Assign lhs $ rhs

dSimple :: Parser ValueExpression
dSimple = do
  expr <- pack <$> identifier
  return $ SimpleReference expr

dFieldRef :: Parser ValueExpression
dFieldRef = try $ do
  typ <- dTypeNameP
  symbol "."
  field <- dTypeNameP
  args <- optional dCallArgs
  return $ TypeMemberRef typ field (fromMaybe [] args)

dCallArgs :: Parser [ValueExpression]
dCallArgs = do
  symbol "("
  args <- sepBy dValueP $ symbol ","
  symbol ")"
  return args

dIndexArgs :: Parser [ValueExpression]
dIndexArgs = do
  symbol "["
  args <- sepBy dValueP $ symbol ","
  symbol "]"
  return args

dValueP :: Parser ValueExpression
dValueP = do
    lhs <- dParens <|> dLiteral <|> dFieldRef <|> dSimple
    args <- optional dCallArgs
    let lhs' = if isJust args then
                 FunctionCall lhs (fromMaybe [] args)
               else
                 lhs

    index <- optional dIndexArgs
    let lhs'' = if isJust index then
                 IndexCall lhs' (fromMaybe [] index)
               else
                 lhs'

    rhs <- many dBoolean
    return $ foldl (
      \a x -> Operation a (fst x) (snd x)) lhs'' rhs
  where
    dParens = do
      symbol "("
      lhs <- dValueP
      symbol ")"
      return lhs

    dBoolean = do
      sym <- pack <$> ( symbol "and"
              <|> symbol "<>"
              <|> symbol "as"
              <|> symbol "<"
              <|> symbol "+"
              <|> symbol "-"
              <|> symbol ">"
              <|> symbol "."
            )
      rhs <- dValueP
      return (sym, rhs)

    dLiteral = dInteger <|> dNil

    dInteger = do
      int <- integer
      return $ Integer int

    dNil = do
      rword "nil"
      return Nil


dFunctionCallP :: Parser [ValueExpression]
dFunctionCallP = do
  symbol "("
  value <- sepBy  dValueP (symbol ",")
  symbol ")"
  return value

dAdditionalCompoundValue :: Parser (Text, ValueExpression)
dAdditionalCompoundValue = do
  sym <- pack <$> (symbol "<>"
                  <|> symbol "="
                  <|> symbol "and"
                  <|> symbol "as"
                  <|> symbol "."
                  <|> symbol ",")
                  
  rhs <- dValueP
  return (sym, rhs)

dParensValue :: Parser ValueExpression
dParensValue = do
  symbol "("
  value <- dValueP
  symbol ")"
  return value
  

dIfExpression :: Parser Expression
dIfExpression = do
  rword "if"
  expr <- dValueP
  rword "then"
  statement <- dStatementP
  elseStatement <- optional ((rword "else") *> dStatementP)
  return $ If expr (Then statement)

dProcedureImplementationP =
   dMemberImplementationP "procedure" (
      \a b c d e ->  MemberProcedureImpl a b c e)
dConstructorImplementationP =
   dMemberImplementationP "constructor" (
      \a b c d e ->  MemberConstructorImpl a b c e)
dDestructorImplementationP =
   dMemberImplementationP "destructor" (
      \a b c d e -> MemberDestructorImpl a b c e)
dFunctionImplementationP =
  dMemberImplementationP "function" MemberFunctionImpl

dMemberImplementationP ::
  String ->
  (TypeName -> TypeName -> [Argument] -> TypeName -> Expression -> ImplementationSpec) ->
  Parser ImplementationSpec
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


dPropertyP :: Parser Field
dPropertyP = do
  rword "property"
  name <- pack <$> identifier
  args <- (fromMaybe []) <$> optional (dFunctionOrProcedureArgs "[" "]")
  symbol ":"
  r <- dTypeNameP
  read <- (pack <$>) <$> optional ( rword "read" *> identifier <* semi )
  write <- (pack <$>) <$> optional (rword "write" *> identifier <* semi)
  annotations <- dAnnotationP
  return $ IndexProperty name (head args) r read write annotations

dProcedureP' :: String -> (Name -> [Argument] -> TypeName -> [Annotation] -> Field) -> Parser Field
dProcedureP' a b = do
  rword a
  name <- identifier
  args <- dFunctionOrProcedureArgs'
  semi
  annotation <- dAnnotationP
  return $ b (pack name) args UnspecifiedType annotation

dAnnotationP :: Parser [Annotation]
dAnnotationP = many (
    (rword "override" *> semi *> pure Override)
  <|> (rword "virtual" *> semi *> pure Virtual)
  <|> (rword "default" *> semi *> pure Default)
  ) 

dConstructorFieldP :: Parser Field
dConstructorFieldP = dProcedureP' "constructor" ( \a b c d -> Constructor a b)

dDestructorFieldP :: Parser Field
dDestructorFieldP = dProcedureP' "destructor" ( \a b c d -> Destructor a d)

dProcedureP :: Parser Field
dProcedureP = dProcedureP' "procedure" ( \a b c d -> Procedure a b d)

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
  functions <- many (dFunctionImplementationP
               <|> dProcedureImplementationP
               <|> dConstructorImplementationP 
               <|> dDestructorImplementationP)
  return $ Implementation $ functions

dUnitInitializationP :: Parser Initialization
dUnitInitializationP = do
  optional $ rword "initialization"
  return $ Initialization

dUnitFinalizationP :: Parser Finalization
dUnitFinalizationP = do
  optional $ rword "finalization"
  return $ Finalization

parseDelphiUnit :: String -> IO ()
parseDelphiUnit = parseTest' dUnitP
