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
  showDelphi (Uses a) =
    let
      a' = map (intercalate ".") a
    in
      "uses\n    " <> intercalate ",\n    " a' <> ";\n"

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
  showDelphi (L a) = "[" <> intercalate ", " (map showDelphi a) <> "]"
  showDelphi (P a) = "(" <> intercalate ", " (map showDelphi a) <> ")"
  showDelphi (F a) = pack $ show a
  showDelphi (T a) = showDelphi a
  showDelphi (I a) = pack $ show a
  showDelphi (S a) = pack $ show a
  showDelphi (RecordValue a) = "("
                            <> intercalate ";n\n  " (map showDelphi a)
                            <> ")"
  showDelphi (Exit Nothing ) = "exit"
  showDelphi (Exit (Just a)) = "exit(" <> showDelphi a <> ")"
  showDelphi (LambdaFunction a b c d) = "function("
                            <> intercalate "; " (map showDelphi a)
                            <> "): "
                            <> showDelphi b
                            <> intercalate "\n" (map showDelphi c)
                            <> " begin\n  "
                            <> showDelphi d
  showDelphi (LambdaProcedure a b c) = "procedure("
                            <> intercalate "; " (map showDelphi a)
                            <> ")\n"
                            <> intercalate "\n" (map showDelphi b)
                            <> " begin\n  "
                            <> showDelphi c
  showDelphi (Inherited (Just a)) = "inherited " <> a
  showDelphi (Inherited Nothing) = "inherited"
  showDelphi (a :| b) = "(" <> showDelphi a <> " or " <> showDelphi b <> ")"
  showDelphi (DTrue) = "True"
  showDelphi (DFalse) = "False"
  showDelphi (Result) = "Result"
  showDelphi (Not a) = "(not " <> showDelphi a <> ")"
  showDelphi (AddressOf a) = "@" <> showDelphi a
  showDelphi (Dereference a) = showDelphi a <> "^"
  showDelphi (a :. b) = showDelphi a <> "." <> showDelphi b
  showDelphi (Is a b) = showDelphi a <> " is " <> showDelphi b
  showDelphi (In a b) = showDelphi a <> " in " <> showDelphi b
  showDelphi (a :$ b) = showDelphi a <> _toDelphiCallArgString(b)
  showDelphi (a :+ b) = "(" <> showDelphi a <> " + " <> showDelphi b <> ")"
  showDelphi (a :== b) = "(" <> showDelphi a <> " = " <> showDelphi b <> ")"
  showDelphi (a :- b) = "(" <> showDelphi a <> " - " <> showDelphi b <> ")"
  showDelphi (a :* b) = "(" <> showDelphi a <> " * " <> showDelphi b <> ")"
  showDelphi (a :/ b) = "(" <> showDelphi a <> " / " <> showDelphi b <> ")"
  showDelphi (a :& b) = "(" <> showDelphi a <> " and " <> showDelphi b <> ")"
  showDelphi (a :< b) = "(" <> showDelphi a <> " < " <> showDelphi b <> ")"
  showDelphi (a :<= b) = "(" <> showDelphi a <> " <= " <> showDelphi b <> ")"
  showDelphi (a :>= b) = "(" <> showDelphi a <> " >= " <> showDelphi b <> ")"
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
  showDelphi (With a b) = "with " <> intercalate "," (map showDelphi a) <> " do\n" <> showDelphi b
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
  showDelphi (If a b c) = "if " <> showDelphi a <> " then\n" <> showDelphi b <> " else " <> showDelphi c
  showDelphi (Begin a) = "begin\n"
                        <> intercalate "\n" (map (\x -> indent <> showDelphi x <> ";") a)
                        <> "\nend"
  showDelphi (ExpressionValue a) = showDelphi a
  showDelphi (EmptyExpression) = ""
  showDelphi (For a LoopUpTo b c) = "for " <> showDelphi a <> " to " <> showDelphi b <> " " <> showDelphi c
  showDelphi (For a LoopDownTo b c) = "for " <> showDelphi a <> " downto " <> showDelphi b <> " " <> showDelphi c
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
                                      <> ": " <> showDelphi c <> ";\n"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberFunctionImpl a b c d e f g) = "function "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c
                                      <> ": " <> showDelphi d <> ";\n"
                                      <> intercalate "\n" (map showDelphi e)
                                      <> intercalate "\n" (map showDelphi f)
                                      <> showDelphi g
                                      <> ";\n"
  showDelphi (MemberProcedureImpl a b c d e f) = "procedure "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";\n"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberConstructorImpl a b c d e f) = "constructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b
                                      <> _toDelphiArgString c <> ";\n"
                                      <> intercalate "\n" (map showDelphi d)
                                      <> intercalate "\n" (map showDelphi e)
                                      <> showDelphi f
                                      <> ";\n"
  showDelphi (MemberDestructorImpl a b c d e) = "destructor "
                                      <> showDelphi a
                                      <> "."
                                      <> showDelphi b <> ";\n"
                                      <> intercalate "\n" (map showDelphi c)
                                      <> intercalate "\n" (map showDelphi d)
                                      <> showDelphi e
                                      <> ";\n"

instance ShowDelphi InterfaceExpression where
  showDelphi (TypeDefinitions a) = "type\n  "
                                 <> intercalate ";\n  " (map showDelphi a) <> ";\n"
  showDelphi (ConstDefinitions a) = "const\n  "
                                <> intercalate ";\n  " (map showDelphi a) <> ";\n"
  showDelphi (VarDefinitions a) = "var\n  "
                                <> intercalate ";\n  " (map showDelphi a) <> ";\n"
  showDelphi (Standalone a) = showDelphi a

instance ShowDelphi ConstDefinition where
  showDelphi (ConstDefinition a (Just b) c) = a <> " : " <> showDelphi b <> " = " <> showDelphi c
  showDelphi (ConstDefinition a (Nothing) c) = a <> " = " <> showDelphi c

instance ShowDelphi VarDefinition where
  showDelphi (VarDefinition a b (Just c)) = a <> ": " <> showDelphi b <> "=" <> showDelphi c
  showDelphi (VarDefinition a b Nothing) = a <> ": " <> showDelphi b

instance ShowDelphi TypeDefinition where
  showDelphi (TypeDef a b) = showDelphi a <> " = "
                           <> showDelphi b
  showDelphi (ForwardClass) = "class"
  showDelphi (Record a b) = showDelphi a
                           <> " = record\n  "
                           <> intercalate "\n  " (
                                map showDelphi b)
                           <> "\n  end"
  showDelphi (Class a b c) = showDelphi a
                           <> " = class("
                              <> intercalate ", " (map showDelphi b)
                           <> ")"
                           <> intercalate "" (
                                map showDelphi c)
                           <> "\n  end"
  showDelphi (TypeAlias a b) = showDelphi a <> " = " <> showDelphi b
  showDelphi (EnumDefinition a b) = showDelphi a <> " = ("
                                  <> intercalate ", " b
                                  <> ")"
  showDelphi (SetDefinition a b) = showDelphi a <> " = set of " <> showDelphi b
  showDelphi (TypeAttribute a b) = "[" <> intercalate ", " (map showDelphi a) <> "]\n  " <> showDelphi b

instance ShowDelphi ArrayIndex where
  showDelphi (IndexOf a) = intercalate ", " (map showDelphi a)
  showDelphi (Range a) = intercalate "," $ map (\(x, y) -> showDelphi x <> ".." <> showDelphi y) a

instance ShowDelphi TypeName where
  showDelphi (Type a)  = a
  showDelphi (ConstType) = "const"
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
  showDelphi (ReferenceToProcedure a) = "{$IFNDEF FPC}reference to{$ENDIF} procedure("
                                      <> intercalate "; " (map showDelphi a)
                                      <> ")"
  showDelphi (SimpleProcedure a) = "procedure("
                                 <> intercalate "; " (map showDelphi a)
                                 <> ")"
  showDelphi (ProcedureOfObject a) = "procedure("
                                 <> intercalate "; " (map showDelphi a)
                                 <> ") of object"
  showDelphi (ReferenceToFunction a b) = "{$IFNDEF FPC}reference to{$ENDIF} procedure("
                                      <> intercalate "; " (map showDelphi a)
                                      <> "):"
                                      <> showDelphi b
  showDelphi (SimpleFunction a b) = "procedure("
                                 <> intercalate "; " (map showDelphi a)
                                 <> "):"
                                 <> showDelphi b
  showDelphi (FunctionOfObject a b) = "procedure("
                                 <> intercalate "; " (map showDelphi a)
                                 <> "):"
                                 <> showDelphi b
                                 <> " of object"
  showDelphi (NewType a) = "type " <> showDelphi a
  showDelphi (ClassOf a) = "class of " <> showDelphi a

instance ShowDelphi Accessibility where
  showDelphi (Private a) = "\n  private\n    " <> intercalate "\n    " (map showDelphi a)
  showDelphi (Public a) = "\n  public\n    "   <> intercalate "\n    " (map showDelphi a)
  showDelphi (Protected a) = "\n  protected\n    " <> intercalate "\n    " (map showDelphi a)
  showDelphi (Published a) = "\n  published\n    " <> intercalate "\n    " (map showDelphi a)
  showDelphi (DefaultAccessibility a) = "\n    " <> intercalate "\n    " (map showDelphi a)

_toDelphiCallArgString :: ShowDelphi a => [a] -> Text
_toDelphiCallArgString x
  | not (null x) = "(" <> (intercalate ", " (map showDelphi x)) <> ")"
  | True         = ""
_toDelphiArgString :: ShowDelphi a => [a] -> Text
_toDelphiArgString x
  | not (null x) = "(" <> (intercalate "; " (map showDelphi x)) <> ")"
  | True         = ""

_toDelphiAnnotations :: ShowDelphi a => [a] -> Text
_toDelphiAnnotations x@[_] = " " <> (intercalate "; " (map showDelphi x)) <> ";"
_toDelphiAnnotations _ = ""

instance ShowDelphi Field where
  showDelphi (Constructor a b c) = "constructor " <> showDelphi a <> _toDelphiArgString b <> ";" <> _toDelphiAnnotations c
  showDelphi (Field a b) = a <> ": " <> showDelphi b <> ";"
  showDelphi (Destructor a b) = "destructor " <> showDelphi a <> ";" <> _toDelphiAnnotations b
  showDelphi (Procedure a b c) = "procedure " <> showDelphi a <> _toDelphiArgString b <> ";"
                               <> _toDelphiAnnotations c
  showDelphi (InheritedProperty a) = "property " <> a <> ";"
  showDelphi (Function a b c d) =
    let static      = if Static `elem` d then "class " else ""
        annotations = _toDelphiAnnotations $ filter (\x -> x /= Static) d
    in  static
        <> "function "
        <> (showDelphi a)
        <> _toDelphiArgString b
        <> ": "
        <> (showDelphi c)
        <> ";"
        <> annotations
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
                                          specs = intercalate " " (map showDelphi e)
                                          def (True) = "; default"
                                          def (False) = ""
                                      in
                                        "property " <> a <> arg <> ": " <> showDelphi c <> index <> " " <> specs <> def f <> ";"

instance ShowDelphi PropertySpecifier where
  showDelphi (PropertyRead a) = "read " <> intercalate "." a
  showDelphi (PropertyWrite a) = "write " <> intercalate "." a
  showDelphi (PropertyStored) = "stored"
  showDelphi (PropertyDefault a) = "default " <> showDelphi a
  showDelphi (PropertyNoDefault) = "nodefault"

instance ShowDelphi FieldAnnotation where
  showDelphi (Override) = "override"
  showDelphi (Virtual) = "virtual"
  showDelphi (Default) = "default"
  showDelphi (StdCall) = "stdcall"
  showDelphi (Dynamic) = "dynamic"
  showDelphi (Static) = "class"
  showDelphi (Overload) = "overload"
  showDelphi (Reintroduce) = "reintroduce"
  showDelphi (Abstract) = "abstract"
  showDelphi (Message a) = "message " <> a

instance ShowDelphi GenericConstraint where
  showDelphi (ClassConstraint) = "class"


instance ShowDelphi ArgModifier where
  showDelphi (ConstArg) = "const "
  showDelphi (VarArg) = "var "
  showDelphi (OutArg) = "out "
  showDelphi (NormalArg) = ""
  

instance ShowDelphi Argument where
  showDelphi (Arg m a (Just b) (Just c)) = showDelphi m <> a <> ": " <> (showDelphi b) <> " = " <> showDelphi c
  showDelphi (Arg m a (Just b) Nothing) = showDelphi m <> a <> ": " <> (showDelphi b)

