{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module AstPrettyPrint where

import           Data.Text                      ( pack
                                                , Text
                                                , intercalate
                                                )
import           Data.List                      ( foldl' )
import           DelphiAst

showArgs :: forall a . (Show a) => [a] -> Text
showArgs args = (pack . show) args
showClassDef :: ClassDefinition -> Text
showClassDef def = (pack . show) def
showTypeName :: TypeName -> Text
showTypeName (Type (Lexeme _ name)) = name
showTypeName name                   = (pack . show) name
showTypeRhs :: TypeDefinitionRHS -> Text
showTypeRhs (ReferenceToProcedure args) =
  "procedure(" <> showArgs args <> ") -- reference"
showTypeRhs (SimpleProcedure args) = "procedure(" <> showArgs args <> ")"
showTypeRhs (ProcedureOfObject args) =
  "procedure(" <> showArgs args <> ") -- of object"
showTypeRhs (ReferenceToFunction args name) =
  "function " <> showTypeName name <> "(" <> showArgs args <> ") -- reference"
showTypeRhs (SimpleFunction args name) =
  "function " <> showTypeName name <> "(" <> showArgs args <> ")"
showTypeRhs (FunctionOfObject args name) =
  "function " <> showTypeName name <> "(" <> showArgs args <> ") -- of object"
showTypeRhs (NewType name) = "type " <> showTypeName name
showTypeRhs (ClassOf name) = "class of " <> showTypeName name
showTypeRhs (ClassHelper name classDef) =
  "class helper for " <> showTypeName name <> showClassDef classDef

showType :: TypeDefinition -> Text
showType (TypeDef name rhs) = intercalate " " [name', rhs']
 where
  name' = showTypeName name
  rhs'  = showTypeRhs rhs
showType (TypeAlias name newName) = "alias " <> name' <> " -> " <> newName'
 where
  name'    = showTypeName name
  newName' = showTypeName newName
showType (EnumDefinition name items) =
  "enum " <> name' <> " = (" <> items' <> ")"
 where
  name' = showTypeName name
  items' :: Text
  items' = intercalate ", " $ foldl' (\b (Lexeme _ a) -> b <> [a]) [] items
showType a = (pack . show) a
