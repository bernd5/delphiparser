{-# LANGUAGE OverloadedStrings #-}

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
  showDelphi (Interface a b)
    =  "interface\n\n"
    <> showDelphi a
    <> intercalate "\n" (map showDelphi b) <> "\n"

instance ShowDelphi Uses where
  showDelphi (Uses []) = ""
  showDelphi (Uses a) = "uses\n    "
    <> intercalate ",\n    " a <> ";\n"

instance ShowDelphi Implementation where
  showDelphi (Implementation a b) = "implementation\n\n"
                                <> showDelphi a
                                <> intercalate "\n" (map showDelphi b)

instance ShowDelphi Initialization where
  showDelphi (Initialization) = "\n\n"
  
instance ShowDelphi Finalization where
  showDelphi (Finalization) = "\n\n"

instance ShowDelphi ValueExpression where
  showDelphi (V a) = a
  showDelphi (T a) = showDelphi a
  showDelphi (I a) = pack $ show a
  showDelphi (S a) = pack $ show a
  showDelphi (Inherited a) = "inherited " <> a
  showDelphi (a :| b) = "(" <> showDelphi a <> " or " <> showDelphi b <> ")"
  showDelphi (DTrue) = "True"
  showDelphi (DFalse) = "False"
  showDelphi (Result) = "Result"
  showDelphi (Not a) = "(not " <> showDelphi a <> ")"
  showDelphi (AddressOf a) = "@" <> showDelphi a
  showDelphi (Dereference a) = "^" <> showDelphi a
  showDelphi (a :. b) = showDelphi a <> "." <> showDelphi b
  showDelphi (Is a b) = showDelphi a <> " is " <> showDelphi b
  showDelphi (In a b) = showDelphi a <> " in " <> showDelphi b
  showDelphi (a :$ b) = showDelphi a <> _toDelphiArgString(b)
  showDelphi (a :+ b) = "(" <> showDelphi a <> " + " <> showDelphi b <> ")"
  showDelphi (a :== b) = "(" <> showDelphi a <> " == " <> showDelphi b <> ")"
  showDelphi (a :- b) = "(" <> showDelphi a <> " - " <> showDelphi b <> ")"
  showDelphi (a :* b) = "(" <> showDelphi a <> " * " <> showDelphi b <> ")"
  showDelphi (a :/ b) = "(" <> showDelphi a <> " / " <> showDelphi b <> ")"
  showDelphi (a :& b) = "(" <> showDelphi a <> " and " <> showDelphi b <> ")"
  showDelphi (a :< b) = "(" <> showDelphi a <> " < " <> showDelphi b <> ")"
  showDelphi (a :<= b) = "(" <> showDelphi a <> " <= " <> showDelphi b <> ")"
  showDelphi (a :> b) = "(" <> showDelphi a <> " > " <> showDelphi b <> ")"
  showDelphi (a :<> b) = "(" <> showDelphi a <> " <> " <> showDelphi b <> ")"
  showDelphi (a `As` b) = "(" <> showDelphi a <> " as " <> showDelphi b <> ")"
  showDelphi (a :<<>> b) = showDelphi a <> "<" <> intercalate "," (map showDelphi b) <> ">"
  showDelphi (a :!! b) = showDelphi a <> "[" <> intercalate "," (map showDelphi b) <> "]"
  showDelphi (Nil) = "nil"

instance ShowDelphi Except where
  showDelphi (ExceptOn (Just a) b) = "except on " <> showDelphi a <> "\n"
                            <> intercalate "\n" (map showDelphi b)
  showDelphi (ExceptOn Nothing b) = "except\n"
                            <> intercalate "\n" (map showDelphi b)
  showDelphi (ExceptElse b) = "else\n"
                            <> intercalate "\n" (map showDelphi b)

instance ShowDelphi CaseBranches where
  showDelphi (CaseBranch a b) = intercalate ", " (map showDelphi a)
                              <> ": " <> showDelphi b

instance ShowDelphi Expression where
  showDelphi (Expr a) = a
  showDelphi (Raise a) = "raise " <> showDelphi a
  showDelphi (With a b) = "with " <> showDelphi a <> " do\n" <> showDelphi b
  showDelphi (Try a (Left b)) = intercalate "\n"
                          [ "try"
                          , intercalate "\n" (map showDelphi a)
                          , intercalate "\n" (map showDelphi b)
                          ]
  showDelphi (Try a (Right b)) = intercalate "\n"
                          [ "try"
                          , intercalate "\n" (map showDelphi a)
                          , "finally"
                          , intercalate "\n" (map showDelphi b)
                          ]
  showDelphi (Case a b c) = "case " <> showDelphi a <> " of \n"
                          <> intercalate "\n" (map showDelphi b)
                          <> fromMaybe "" c'
    where
      c' = (\(Else x) -> "else\n" <> showDelphi x) <$> c
  showDelphi (a := b) = showDelphi a <> " := " <> showDelphi b
  showDelphi (If a b (Else EmptyExpression)) = "if " <> showDelphi a <> " then\n" <> showDelphi b
  showDelphi (If a b c) = "if " <> showDelphi a <> " then " <> showDelphi b <> " else " <> showDelphi c
  showDelphi (Begin a) = "\nbegin\n"
                        <> intercalate "\n" (map (\x -> indent <> showDelphi x <> ";") a)
                        <> "\nend"
  showDelphi (ExpressionValue a) = showDelphi a
  showDelphi (EmptyExpression) = ""
  showDelphi (For a LoopUpTo b c) = "for " <> showDelphi a <> " to " <> showDelphi b <> showDelphi c
  showDelphi (For a LoopDownTo b c) = "for " <> showDelphi a <> " downto " <> showDelphi b <> showDelphi c
  showDelphi (While a b) = "while (" <> showDelphi a <> ") do\n" <> showDelphi b
  showDelphi (Repeat a b) = "repeat " <> (intercalate "\n" (map showDelphi a) ) <> "\nuntil\n" <> showDelphi b

instance ShowDelphi Then where
  showDelphi (Then a) = showDelphi a

instance ShowDelphi Else where
  showDelphi (Else a) = showDelphi a

instance ShowDelphi ImplementationSpec where
  showDelphi (AdditionalInterface a) = showDelphi a
  showDelphi (ProcedureImpl a b c d e) = "function "
                                      <> showDelphi a
                                      <> _toDelphiArgString b
                                      <> ";\n"
                                      <> intercalate "\n" (map showDelphi c)
                                      <> intercalate "\n" (map showDelphi d)
                                      <> showDelphi e
                                      <> ";\n"
  showDelphi (FunctionImpl a b c d e f) = "function "
                                      <> showDelphi a
                                      <> _toDelphiArgString b
                                      <> ": " <> showDelphi c <> ";"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberFunctionImpl a b c d e f g) = "function "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c
                                      <> ": " <> showDelphi d <> ";"
                                      <> intercalate "\n" (map showDelphi e)
                                      <> intercalate "\n" (map showDelphi f)
                                      <> showDelphi g
                                      <> ";\n"
  showDelphi (MemberProcedureImpl a b c d e f) = "procedure "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberConstructorImpl a b c d e f) = "constructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberDestructorImpl a b c d e) = "destructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b <> ";"
                                      <> intercalate "\n" (map showDelphi c)
                                      <> intercalate "\n" (map showDelphi d)
                                      <> showDelphi e
                                      <> ";\n"

instance ShowDelphi InterfaceExpression where
  showDelphi (TypeDefinitions a) = "type\n"
                                 <> intercalate "\n" (map showDelphi a)
  showDelphi (ConstDefinitions a) = "const\n"
                                <> intercalate "\n" (map showDelphi a)
  showDelphi (VarDefinitions a) = "var\n"
                                <> intercalate "\n" (map showDelphi a)

instance ShowDelphi ConstDefinition where
  showDelphi (ConstDefinition a (Just b) c) = a <> " : " <> showDelphi b <> " = (" <> intercalate ", " (map showDelphi c) <> ")"
  showDelphi (ConstDefinition a (Nothing) c) = a <> " = (" <> intercalate ", " (map showDelphi c) <> ")"

instance ShowDelphi VarDefinition where
  showDelphi (VarDefinition a b) = a <> " : " <> showDelphi b

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
  showDelphi (Type a)  = a
  showDelphi (StaticArray a b) = "array[" <> showDelphi a <> "] of " <> showDelphi b
  showDelphi (DynamicArray a b) = (pack . concat $ take a' $ repeat "array of ") <> showDelphi b
    where
      a' = fromInteger a
  showDelphi (VariantArray a) = "array[" <> showDelphi a <> "] of const"
  showDelphi (OpenDynamicArray a) = "array of " <> showDelphi a
  showDelphi (AddressOfType a) = "^" <> showDelphi a
  showDelphi (TargetOfPointer a) = "@" <> showDelphi a
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
  showDelphi UnspecifiedType = "{ Unspecified Type }"

instance ShowDelphi TypeDefinitionRHS where
  showDelphi (UnknownTypeDefinition a) = "{ Unknown Type Definition: " <> a <> " }"
  showDelphi (ReferenceToProcedure a) = "{$IFNDEF FPC}reference to{$ENDIF} procedure("
                                      <> intercalate ", " (map showDelphi a)
                                      <> ");"
  showDelphi (SimpleProcedure a) = "procedure("
                                 <> intercalate ", " (map showDelphi a)
                                 <> ");"
  showDelphi (ProcedureOfObject a) = "procedure("
                                 <> intercalate ", " (map showDelphi a)
                                 <> ") of object;"

instance ShowDelphi Accessibility where
  showDelphi (Private a) = "private\n" <> intercalate "\n" (map (\x -> indent <> showDelphi x) a)
  showDelphi (Public a) = "public\n"   <> intercalate "\n" (map (\x -> indent <> showDelphi x) a) 
  showDelphi (Protected a) = "protected\n" <> intercalate "\n" (map showDelphi a)
  showDelphi (Published a) = "published\n" <> intercalate "\n" (map showDelphi a)
  showDelphi (DefaultAccessibility a) = intercalate "\n" (map showDelphi a)

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
  showDelphi (Property a b c d e f) = let arg = fromMaybe
                                                  ""
                                                  $ (\x -> "[" <> intercalate "," (map showDelphi x) <> "]")
                                                  <$> b
                                          index = fromMaybe
                                                    ""
                                                    $ (\x -> " index " <> showDelphi x)
                                                    <$> d
                                          specs = intercalate ";" (map showDelphi e)
                                          def (True) = "; default"
                                          def (False) = ""
                                      in
                                        "property " <> a <> arg <> ": " <> showDelphi c <> index <> specs <> def f <> ";"

instance ShowDelphi PropertySpecifier where
  showDelphi (PropertyRead a) = "read " <> a
  showDelphi (PropertyWrite a) = "write " <> a
  showDelphi (PropertyStored) = "stored"
  showDelphi (PropertyDefault a) = "default " <> showDelphi a
  showDelphi (PropertyNoDefault) = "nodefault"

instance ShowDelphi FieldAnnotation where
  showDelphi (Override) = "override"
  showDelphi (Virtual) = "virtual"
  showDelphi (Default) = "default"
  showDelphi (StdCall) = "stdcall"
  showDelphi (Static) = "class"

instance ShowDelphi GenericConstraint where
  showDelphi (ClassConstraint) = "class"


instance ShowDelphi ArgModifier where
  showDelphi (ConstArg) = "class"
  showDelphi (VarArg) = "var"
  showDelphi (OutArg) = "out"
  showDelphi (NormalArg) = ""
  

instance ShowDelphi Argument where
  showDelphi (Arg m a b (Just c)) = showDelphi m <> a <> ": " <> (showDelphi b) <> "=" <> showDelphi c
  showDelphi (Arg m a b Nothing) = showDelphi m <> a <> ": " <> (showDelphi b)

