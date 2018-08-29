{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module DelphiWriter where

import DelphiAst
import Data.Text (Text, pack, intercalate)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

indent :: Text
indent = "  "

class ShowDelphi a where
  showDelphi :: a -> Text

instance ShowDelphi Unit where
  showDelphi (Unit a b c d e) =
    "unit " <> a <> ";\n{$mode delphi}\n\n" <>
    (showDelphi b) <>
    (showDelphi c) <>
    (showDelphi d) <>
    (showDelphi e) <>
    "end.\n"

instance ShowDelphi Interface where
  showDelphi (Interface (Uses []) b)
    =  "interface\n\n"
    <> intercalate "\n" (map showDelphi b) <> "\n"
  showDelphi (Interface (Uses a) b)
    =  "interface\n\n"
    <> "uses\n"
    <> intercalate ",\n" a <> ";\n"
    <> intercalate "\n" (map showDelphi b) <> "\n"

instance ShowDelphi Implementation where
  showDelphi (Implementation a) = "implementation\n\n"
                                <> intercalate "\n" (map showDelphi a)

instance ShowDelphi Initialization where
  showDelphi (Initialization) = "\n\n"
  
instance ShowDelphi Finalization where
  showDelphi (Finalization) = "\n\n"

instance ShowDelphi ValueExpression where
  showDelphi (V a) = a
  showDelphi (T a) = showDelphi a
  showDelphi (I a) = pack $ show a
  showDelphi (a :. b) = showDelphi a <> "." <> showDelphi b
  showDelphi (a :$ b) = showDelphi a <> _toDelphiArgString(b)
  showDelphi (a :+ b) = "(" <> showDelphi a <> " + " <> showDelphi b <> ")"
  showDelphi (a :- b) = "(" <> showDelphi a <> " - " <> showDelphi b <> ")"
  showDelphi (a :* b) = "(" <> showDelphi a <> " * " <> showDelphi b <> ")"
  showDelphi (a :/ b) = "(" <> showDelphi a <> " / " <> showDelphi b <> ")"
  showDelphi (a :& b) = "(" <> showDelphi a <> " and " <> showDelphi b <> ")"
  showDelphi (a :< b) = "(" <> showDelphi a <> " < " <> showDelphi b <> ")"
  showDelphi (a :> b) = "(" <> showDelphi a <> " > " <> showDelphi b <> ")"
  showDelphi (a :<> b) = "(" <> showDelphi a <> " <> " <> showDelphi b <> ")"
  showDelphi (a `As` b) = "(" <> showDelphi a <> " as " <> showDelphi b <> ")"
  showDelphi (a :<<>> b) = showDelphi a <> "<" <> intercalate "," (map showDelphi b) <> ">"
  showDelphi (a :!! b) = showDelphi a <> "[" <> intercalate "," (map showDelphi b) <> "]"
  showDelphi (Nil) = "nil"

instance ShowDelphi Expression where
  showDelphi (Expr a) = a
  showDelphi (a := b) = showDelphi a <> " := " <> showDelphi b
  showDelphi (If a b (Else EmptyExpression)) = "if " <> showDelphi a <> " then " <> showDelphi b
  showDelphi (If a b c) = "if " <> showDelphi a <> " then " <> showDelphi b <> " else " <> showDelphi c
  showDelphi (Begin a) = "\nbegin\n"
                        <> intercalate "\n" (map (\x -> indent <> showDelphi x <> ";") a)
                        <> "\nend"
  showDelphi (ExpressionValue a) = showDelphi a
  showDelphi (EmptyExpression) = ""

instance ShowDelphi Then where
  showDelphi (Then a) = showDelphi a

instance ShowDelphi Else where
  showDelphi (Else a) = showDelphi a

instance ShowDelphi ImplementationSpec where
  showDelphi (FunctionImpl a b c d) = "function "
                                      <> showDelphi a
                                      <> _toDelphiArgString b
                                      <> ": " <> showDelphi c <> ";"
                                      <> showDelphi d
                                      <> ";\n"
  showDelphi (MemberFunctionImpl a b c d e) = "function "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c
                                      <> ": " <> showDelphi d <> ";"
                                      <> showDelphi e
                                      <> ";\n"
  showDelphi (MemberProcedureImpl a b c d) = "procedure "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";"
                                      <> showDelphi d
                                      <> ";\n"
  showDelphi (MemberConstructorImpl a b c d) = "constructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";"
                                      <> showDelphi d
                                      <> ";\n"
  showDelphi (MemberDestructorImpl a b c) = "destructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b <> ";"
                                      <> showDelphi c
                                      <> ";\n"

instance ShowDelphi InterfaceExpression where
  showDelphi (TypeDefinitions a) = "type\n"
                                 <> intercalate "\n" (map showDelphi a)
  showDelphi (ConstDefinitions a) = "const\n"
                                <> intercalate "\n" (map showDelphi a)

instance ShowDelphi ConstDefinition where
  showDelphi (ConstDefinition a b) = a <> " = " <> intercalate "," (map showDelphi b)

instance ShowDelphi TypeDefinition where
  showDelphi (TypeDef a b) = showDelphi a <> " = "
                           <> showDelphi b <> "\n"
  showDelphi (ForwardClass) = "class"
  showDelphi (Record a b) = showDelphi a
                           <> " = record\n"
                           <> intercalate "\n" (
                                map showDelphi b)
                           <> "\nend;\n"
  showDelphi (Class a b c) = showDelphi a
                           <> " = class("
                              <> intercalate ", " (map showDelphi b)
                           <> ")\n"
                           <> intercalate "\n" (
                                map showDelphi c)
                           <> "\nend;\n"
  showDelphi (TypeAlias a b) = showDelphi a <> " = " <> showDelphi b
  showDelphi (EnumDefinition a b) = showDelphi a <> " = ("
                                  <> intercalate ", " b
                                  <> ")"
  showDelphi (SetDefinition a b) = showDelphi a <> " = set of " <> showDelphi b

instance ShowDelphi ArrayIndex where
  showDelphi (IndexOf a) = showDelphi a
  showDelphi (Range a) = intercalate "," $ map (\(x, y) -> pack ( show x ) <> ".." <> pack ( show y) ) a

instance ShowDelphi TypeName where
  showDelphi UnspecifiedType = "{ Unspecified Type }"
  showDelphi (Type a)  = a
  showDelphi (StaticArray a b) = "array[" <> showDelphi a <> "] of " <> showDelphi b
  showDelphi (DynamicArray a b) = "array of " <> showDelphi b -- TODO: FIx this, needs to repeat.
  showDelphi (VariantArray a) = "array of const" -- TODO: Fix this, needs to repeat.
  showDelphi (OpenDynamicArray a) = "array of " <> showDelphi a
  showDelphi (Constraint a) = "Constraint "
                             <> intercalate "\n" ( map showDelphi a)
  showDelphi (GenericDefinition a b) = a <> "<"
                                      <> intercalate ", " (map showDelphi b)
                                      <> ">"
  showDelphi (GenericMethodOfType a b) = showDelphi a <> "<" <> showDelphi b <> ">"
  showDelphi (GenericInstance a b) = a
                                    <> "<"
                                    <> intercalate ", "(map showDelphi b) 
                                    <> ">"

instance ShowDelphi TypeDefinitionRHS where
  showDelphi (UnknownTypeDefinition a) = "{ Unknown Type Definition: " <> a <> " }"
  showDelphi (ReferenceToProcedure a) = "{$IFNDEF FPC}reference to{$ENDIF} procedure("
                                      <> intercalate ", " (map showDelphi a)
                                      <> ");"

instance ShowDelphi Accessibility where
  showDelphi (Private a) = "private\n" <> intercalate "\n" (map (\x -> indent <> showDelphi x) a)
  showDelphi (Public a) = "public\n"   <> intercalate "\n" (map (\x -> indent <> showDelphi x) a) 
  showDelphi (Protected a) = "protected\n" <> intercalate "\n" (map showDelphi a)
  showDelphi (Published a) = "published\n" <> intercalate "\n" (map showDelphi a)

_toDelphiArgString :: ShowDelphi a => [a] -> Text
_toDelphiArgString x | not (null x) = "(" 
                                      <> (intercalate
                                          ", "
                                          ( map showDelphi x) ) 
                                      <> ")"
                     | True         = ""

_toDelphiAnnotations :: ShowDelphi a => [a] -> Text
_toDelphiAnnotations x = foldl (\e' e -> e' <> " " <> (showDelphi e) <> ";") "" x

instance ShowDelphi Field where
  showDelphi (Constructor a b) = "constructor " <> a <> _toDelphiArgString b <> ";"
  showDelphi (Field a b) = a <> ": " <> showDelphi b <> ";"
  showDelphi (Destructor a b) = "destructor " <> a <> ";" <> _toDelphiAnnotations b
  showDelphi (Procedure a b c) = "procedure " <> a <> _toDelphiArgString b <> ";"
                                <> foldl (\d' d -> d' <> ( showDelphi d) <> ";") "" c
  showDelphi (Function a b c d) = "function " <> (showDelphi a) <> _toDelphiArgString b <> ": "
                                <> (showDelphi c) <> ";"
                                <> _toDelphiAnnotations d
  showDelphi (IndexProperty a (Just b) c d e f g h) = "property " <> a
                                            <> "[" <> showDelphi b <> "]: "
                                            <> showDelphi c
                                            <> fromMaybe "" ((\x -> " index " <> x) <$> d)
                                            <> fromMaybe "" ((\x -> " read " <> x) <$> e)
                                            <> fromMaybe "" ((\x -> " write " <> x) <$> f)
                                            <> fromMaybe "" ((\x -> " default " <> showDelphi x) <$> g)
                                            <> ";"
                                            <> _toDelphiAnnotations h
  showDelphi (IndexProperty a (Nothing) c d e f g h) = "property " <> a
                                            <> showDelphi c
                                            <> fromMaybe "" ((\x -> " index " <> x) <$> d)
                                            <> fromMaybe "" ((\x -> " read " <> x) <$> e)
                                            <> fromMaybe "" ((\x -> " write " <> x) <$> f)
                                            <> fromMaybe "" ((\x -> " default " <> showDelphi x) <$> g)
                                            <> ";"
                                            <> _toDelphiAnnotations h

instance ShowDelphi Annotation where
  showDelphi (Override) = "override"
  showDelphi (Virtual) = "virtual"
  showDelphi (Default) = "default"

instance ShowDelphi GenericConstraint where
  showDelphi (ClassConstraint) = "class"

instance ShowDelphi Argument where
  showDelphi (Arg a b) = a <> ": " <> (showDelphi b)

