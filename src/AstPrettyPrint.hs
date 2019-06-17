{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module AstPrettyPrint where

import           Data.Text                      ( pack
                                                , Text
                                                , intercalate
                                                )
import           Data.List                      ( foldl' )
import           DelphiAst

class PP a where
  pp :: a -> Text

instance PP TypeDefinition where
  pp (TypeAlias a b) = (pp a) <> " = " <> (pp b) <> ""
  pp (EnumDefinition a b) = pp a <> " = (" <> intercalate ", " (map pp b) <> ")"
  pp (SetDefinition a b) = pp a <> " = set of " <> pp b
  pp (TypeDef a b) = pp a <> " = " <> pp b
  pp a = (pack . show) a

instance PP TypeName where
  pp (Type a) = pp a
  pp (StaticArray a b) = "array[" <> pp a <> "] of " <> pp b
  pp a = (pack . show) a

instance PP ValueExpression where
  pp (I a) = pp a
  pp (a :.. b) = pp a <> ".." <> pp b
  pp a = (pack . show) a

instance PP ArrayIndex where
  pp (IndexOf a) = intercalate ", " (map pp a)

instance PP (Lexeme Text) where
  pp (Lexeme [] a) = a

instance PP (Lexeme Integer) where
  pp (Lexeme [] a) = (pack . show) a

instance PP TypeDefinitionRHS where
  pp (SimpleProcedure a) = "procedure(" <> intercalate "; " (map pp a) <> ")"
  pp (ProcedureOfObject a) = pp(SimpleProcedure a) <> " of object"
  pp (SimpleFunction a b) = "function(" <> intercalate "; " (map pp a) <> "): " <> pp b
  pp a = (pack . show) a

instance PP ArgModifier where
  pp ConstArg = "const"
  pp VarArg = "var"
  pp OutArg = "out"
  pp NormalArg = ""

instance PP Argument where
  pp (Arg a b Nothing Nothing) = intercalate " " [pp a, pp b]
  pp (Arg a b (Just c) Nothing) = pp (Arg a b Nothing Nothing) <> ": " <> pp c
  pp (Arg a b (Just c) (Just d)) = pp (Arg a b (Just c) Nothing) <> " = " <> pp d

