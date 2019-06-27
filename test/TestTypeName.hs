{-# LANGUAGE OverloadedStrings #-}

module TestTypeName
  ( typeNameTests
  )
where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )

import           DelphiAst
import           DelphiParser                   ( typeName)
import DelphiLexer
import TestSupport

typeNameTests :: TestTree
typeNameTests = testGroup
  "Delphi type name Tests"
  [ testGroup
      "Various comments"
      [ testCase' "foo // bar" typeName $ (Type (Lexeme [Comment " bar"] "foo"))
      , testCase' "{ bar} foo"                     typeName
        $ (DirectiveType (Lexeme [Comment " bar"] (Type (Lexeme [] "foo"))))
      , testCase' "{$bar}"                         typeName
        $ (DirectiveType (Lexeme [UnknownDirective "bar"] UnspecifiedType))
      , testCase' "{$if bar}foo{$endif}"           typeName
        $ (DirectiveType (ifDef' "bar" "foo" "" UnspecifiedType))
      , testCase' "{$if bar}foo{$else}baz{$endif}" typeName
        $ (DirectiveType (ifDef' "bar" "foo" "baz" UnspecifiedType))
      , testCase' "{$if bar}{$if bag}foo{$else}baz{$endif}{$endif}" typeName
        $ (DirectiveType
            (Lexeme [IfDef "bar" (ifDef "bag" "foo" "baz") []] UnspecifiedType)
          )
      , testCase' "$if bar}{$if bag}foo{$else}baz{$endif}{$endif}"
                  compilerDirective
        $ (IfDef "bar" (ifDef "bag" "foo" "baz") [])
      , testCase' "{$if bar}{$if bar}foo{$else}baz{$endif}a{$endif}" typeName
        $ (DirectiveType
            (Lexeme
              [IfDef "bar" ((ifDef "bar" "foo" "baz") <> [Right "a"]) []]
              UnspecifiedType
            )
          )
      , testCase'
          "{$if bar}{foo bar}foo{$else}{bar foo}baz{$endif}a{$endif}"
          typeName
        $ (DirectiveType
            (Lexeme
              [ IfDef "bar"
                      [Left (Comment "foo bar"), Right "foo"]
                      [Left (Comment "bar foo"), Right "baz"]
              ]
              UnspecifiedType
            ) -- Note: 'a{$endif}' is not parsed, as the parser is done.
          )
          , testCase'
              "{$if bar}baz1{foo bar}foo{three}foo{$else sigma}{bar foo}baz{four}{$endif ok}a{$endif}"
              typeName
            $ (DirectiveType
                (Lexeme
                  [ IfDef
                      "bar"
                      [ Right "baz1"
                      , Left (Comment "foo bar")
                      , Right "foo"
                      , Left (Comment "three")
                      , Right "foo"
                      ]
                      [Left (Comment "bar foo"), Right "baz", Left (Comment "four")]
                  ]
                  UnspecifiedType
                ) -- Note: 'a{$endif}' is not parsed, as the parser is done.
                )
          , testCase'
              "{$if bar}baz1{foo bar}foa{three}fob{$else sigma}{bar foo}baz{four}{$endif ok}a{$endif}"
              comment
            $ [ IfDef "bar" [ Right "baz1", Left (Comment "foo bar"), Right "foa", Left (Comment "three"), Right "fob"] [Left (Comment "bar foo"), Right "baz", Left (Comment "four")]
              ]
      , testCase'
          "{$if bar}baz1{$else sigma}{bar foo}baz{four}{$endif ok}a{$endif}"
          comment
        $ [IfDef "bar" [Right "baz1"] [Left (Comment "bar foo"),Right "baz",Left (Comment "four")]]    
      , testCase'
          "{$if bar}baz1{foo bar}foo{$else sigma}{bar foo}baz{four}{$endif ok}a{$endif}"
          comment
          $ [IfDef "bar" [Right "baz1", Left (Comment "foo bar"), Right "foo"] [Left (Comment "bar foo"),Right "baz",Left (Comment "four")]]    
      ]
  ]
