{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module AstPrettyPrint
  ( PP(..)
  )
where

import           Prelude                 hiding ( unwords
                                                , concat
                                                , lines
                                                )
import           Data.Text                      ( pack
                                                , Text
                                                , intercalate
                                                , lines
                                                )
import           DelphiAst

unwords :: [Text] -> Text
unwords = intercalate " "

concat :: [Text] -> Text
concat = intercalate ""

prefix :: Text -> Text -> Text
prefix _ "" = ""
prefix a b  = a <> b

indent :: Text -> Text
indent a = concat $ map ("\n  " <>) $ lines a

class PP a where
  pp :: a -> Text

instance PP Directive where
  pp (Comment a) = "{" <> a <> "}"
  pp (Compound a b) = intercalate "\n" [ pp a
                                       , pp b
                                       ]
  pp (UnknownDirective (a, b)) = "{$" <> a <> b <> "}"
  pp (IfDef name thn els) = "{$ifdef " <> name <> "}"
                              <> concat ( map toText thn )
                              <> (if els == []
                                  then ""
                                  else "{$else}" <> concat ( map toText els )
                                 )
                              <> "{$endif}"
                  where
                      toText :: Either Directive Text -> Text
                      toText (Left a) = pp a
                      toText (Right a) = a

  pp a = pack $ show a

instance PP Uses where
  pp (Uses [] c ) = intercalate "\n" [ pp c ]
  pp (Uses unitNames NoDirective) = intercalate " " [ "uses"
                                                  , intercalate ", " (map pp unitNames) <> ";"
                                                  ]
  pp (Uses unitNames directive) = intercalate " " [ "uses"
                                                  , intercalate ", " (map pp unitNames) <> ";"
                                                  , pp directive
                                                  ]
  pp a = pack $ show a

instance PP VarDefinition where
  pp a = pack $ show a

instance PP ConstDefinition where
  pp a = pack $ show a

instance Show a => PP (Maybe a) where
  pp a = pack $ show a

instance PP Integer where
  pp a = pack $ show a

instance PP Text where
  pp a = a

instance PP TypeDefinition where
  pp (TypeAlias a b) = pp a <> " = " <> pp b <> ""
  pp (EnumDefinition a b) = pp a <> " = (" <> intercalate ", " (map pp b) <> ")"
  pp (SetDefinition a b) = pp a <> " = set of " <> pp b
  pp (TypeDef a b) = pp a <> " = " <> pp b
  pp (Class a b c) = concat [ pp a
                        , " = class("
                        , intercalate ", " $ map pp b
                        , ")"
                        , intercalate "" $ map pp c
                        , "\nend"]
  pp (Record a b) = concat [ pp a
                      , " = record"
                      , (intercalate "" $ map pp b)
                      , "\nend"
                      ]
  pp a = (pack . show) a

instance PP Accessibility where
  pp (Private f) = "\nprivate\n  " <> (intercalate "\n  " $ map pp f)
  pp (Public f) = "\npublic\n  " <> (intercalate "\n  " $ map pp f)
  pp (Protected f) = "\nprotected\n  " <> (intercalate "\n  " $ map pp f)
  pp (Published f) = "\npublished\n  " <> (intercalate "\n  " $ map pp f)
  pp (DefaultAccessibility f) = "\n  " <> (intercalate "\n  " $ map pp f)

instance PP Field where
  pp (Constructor a b c) = concat [ "constructor "
                                   , pp a
                                   , "(", unwords (map pp b), ")"
                                   , prefix "; " (unwords (map pp c))
                                   ]
  pp (Destructor a b) = concat [ "destructor "
                                   , pp a
                                   , prefix "; " (unwords (map pp b))
                                   ]
  pp (Function a b c d) = concat [ "function "
                                 , pp a
                                 , "(", unwords (map pp b), ")"
                                 , ": ", pp c
                                 , prefix "; " (unwords (map pp d))
                                 ]
  pp (Procedure a b c) = concat [ "procedure "
                                 , pp a
                                 , "(", unwords (map pp b), ")"
                                 , prefix "; " (unwords (map pp c))
                                 ]
  pp (Property a (Just index) c (Just defaultValue) e isDefault) = concat ["property "
                                     , pp a
                                     , "["
                                     , intercalate ", " (map pp index)
                                     , "]: "
                                     , pp c
                                     , pp defaultValue
                                     , prefix " " (unwords (map pp e))
                                     , if isDefault then "default" else ""
                                     ]
  pp (Property a (Just index) c Nothing e isDefault) = concat [ "property "
                                     , pp a
                                     , "["
                                     , intercalate ", " (map pp index)
                                     , "]: "
                                     , pp c
                                     , prefix " " (unwords (map pp e))
                                     , if isDefault then "default" else ""
                                     ]
  pp (Field a b) = pp a <> ": " <> pp b

  pp a = (pack . show) a

instance PP PropertySpecifier where
  pp (PropertyRead [a]) = "read " <> pp a
  pp (PropertyWrite [a]) = "write " <> pp a

instance PP FieldAnnotation where
  pp Override = "override"
  pp Static = "static"
  pp Virtual = "virtual"
  pp a = (pack . show) a

instance PP TypeName where
  pp (Type a) = pp a
  pp (StaticArray a b) = "array[" <> pp a <> "] of " <> pp b
  pp (DynamicArray 1 b) = "array of " <> pp b
  pp (DynamicArray a b) = "array of " <> pp (DynamicArray (a - 1) b)
  pp (GenericInstance a b) = pp a <> "<" <> intercalate ", " (map pp b) <> ">"
  pp (GenericDefinition a b) = pp a <> "<" <> intercalate ", " (map pp b) <> ">"
  pp a = (pack . show) a

instance PP ValueExpression where
  pp (I a) = pp a
  pp (a :.. b) = pp a <> ".." <> pp b
  pp a = (pack . show) a

instance PP Expression where
  pp (Begin a) = intercalate "\n" [ "begin"
                                  , indent $ intercalate ";\n" (map pp a )
                                  , "end"
                                  ]
  pp a = (pack . show) a

instance PP ArrayIndex where
  pp (IndexOf a) = intercalate ", " (map pp a)

instance PP (Lexeme Text) where
  pp (Lexeme NoDirective a) = intercalate " " [ a
                                              ]
  pp (Lexeme c a) = intercalate " " [ pack $ show c
                                    , a
                                    ]

instance PP (Lexeme Integer) where
  pp (Lexeme NoDirective a) = (pack . show) a

instance PP TypeDefinitionRHS where
  pp (SimpleProcedure a) = "procedure(" <> intercalate "; " (map pp a) <> ")"
  pp (ProcedureOfObject a) = pp(SimpleProcedure a) <> " of object"
  pp (SimpleFunction a b) = "function(" <> intercalate "; " (map pp a) <> "): " <> pp b
  pp (ClassHelper name defn) = intercalate " " [ "class helper for"
                                               , pp name
                                               , intercalate ";\n" ( map (\x -> pp x <> ";") defn )
                                               ] <> "\nend;"
  pp a = (pack . show) a

instance PP ArgModifier where
  pp ConstArg = "const"
  pp VarArg = "var"
  pp OutArg = "out"
  pp NormalArg = ""

instance PP Unit where
  pp (Unit directive name intf impl init fin) = pp directive <> pp name <> pp intf <> pp impl <> pp init <> pp fin
  pp (Program (Lexeme comments name) (Uses [] NoDirective) implSpecs expr) = intercalate ";\n" [ pp comments
                                                                              , "program " <> pp name
                                                                              , intercalate "\n" (map pp implSpecs)
                                                                              , intercalate "\n" [ "begin"
                                                                                                 , indent $ intercalate "\n" (map pp expr)
                                                                                                 , "end."]
                                                                              ]
  pp (Program (Lexeme comments name) uses implSpecs expr) = intercalate ";\n" [ pp comments
                                                                              , "program " <> pp name
                                                                              , pp uses <> "\n" <> (
                                                                                intercalate "\n" (map pp implSpecs))
                                                                              , intercalate "\n" (map pp expr)
                                                                              ]

instance PP Implementation where
  pp (Implementation uses implSpecs) = "implementation\n" <> pp uses <> intercalate "\n" (map pp implSpecs)

instance PP ImplementationSpec where
  pp (AdditionalInterface intf) = pp intf
  pp (FunctionImpl name args result fields implSpecs expr) = concat  [ "function " <>  pp name
                                                                     , if [] == args
                                                                        then ""
                                                                        else "(" <> ( intercalate "; " (map pp args)  ) <> ")"
                                                                     , "; "
                                                                     , pp result
                                                                     , ";"
                                                                     , if [] == fields
                                                                        then ""
                                                                        else intercalate "; " (map pp fields) <> ";"
                                                                     , "\n"
                                                                     , intercalate ";\n" (map pp implSpecs)
                                                                     , pp expr
                                                                     ]
  pp a = pack $ show a

instance PP Interface where
  pp a = pack $ show a

instance PP InterfaceExpression where
  pp (TypeDefinitions typeDefs) = "type" <> ( indent $ intercalate ";\n" (map pp typeDefs) )

instance PP Initialization where
  pp (Initialization) = "// Initialisation not implemented"

instance PP Finalization where
  pp (Finalization) = "// Finalisation not implemented"

instance PP Argument where
  pp (Arg a b Nothing Nothing) = unwords $ filter (/= "") [pp a, pp b]
  pp (Arg a b (Just c) Nothing) = pp (Arg a b Nothing Nothing) <> ": " <> pp c
  pp (Arg a b (Just c) (Just d)) = pp (Arg a b (Just c) Nothing) <> " = " <> pp d

