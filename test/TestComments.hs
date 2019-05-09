{-# LANGUAGE OverloadedStrings #-}
module TestComments
where

import Test.Tasty
import Test.Tasty.HUnit
import DelphiLexer
import Text.Megaparsec (parse)
import DelphiAst (Directive(..), Lexeme(..))
import TestSupport

commentTests :: TestTree
commentTests = testGroup "c Tests"
  [ testCase "//SimpleLinec NewLine"
  $ (Right [c "SimpleLineComment"] @=? )
  $ parse comment "" "//SimpleLineComment\n"
  , testCase "//SimpleLineComment"
  $ (Right [c "SimpleLineComment"] @=? )
  $ parse comment "" "//SimpleLineComment"
  , testCase "//MultipleLineComment"
  $ (Right [c "MultipleLineComment\nSecondLine!"] @=? )
  $ parse comment "" "//MultipleLineComment\n//SecondLine!"
  , testCase "//MultipleLineComment#2"
  $ (Right [c "MultipleLineComment\nSecondLine!"] @=? )
  $ parse comment "" "//MultipleLineComment\n  //SecondLine!"
  , testCase "{SimpleBlockComment}NewLine"
  $ (Right [c "SimpleBlockComment"] @=? )
  $ parse comment "" "{SimpleBlockComment}\n"
  , testCase "{SimpleBlockComment}"
  $ (Right [c "SimpleBlockComment"] @=? )
  $ parse comment "" "{SimpleBlockComment}"
  , testCase "{SimpleBlockComment}//ThenLine"
  $ (Right [c "SimpleBlockComment", c "ThenLine"] @=? )
  $ parse comment "" "{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  $ (Right [c "SimpleBlockComment", c "SimpleBlockComment", c "ThenLine"] @=? )
  $ parse comment "" "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment} \\n {SimpleBlockComment} //ThenLine"
  $ (Right [c "SimpleBlockComment", c "SimpleBlockComment", c "ThenLine"] @=? )
  $ parse comment "" "{SimpleBlockComment} {SimpleBlockComment} //ThenLine"
  , testCase "Multiple lines..."
  $ (Right [c "--\n This file starts with comments\n--", c " And another comment"] @=? )
  $ parse comment "" "{--\n This file starts with comments\n--}\n// And another comment\n\n\nunit TestUnit; interface type implementation initialization finalization end."
  , testCase "Nested Comments"
  $ (Right [c "hey{there}blah"] @=? )
  $ parse comment "" "{hey{there}blah}"
  , testCase "Commented directive"
  $ (Right [c " $define blah"] @=? )
  $ parse comment "" "{ $define blah}"
  , testCase "Directive: $i foo"
  $ (Right [include "foo"] @=? )
  $ parse comment "" "{$i foo}"
  , testCase' "{$i foo} {bar!}" comment
  $ [include "foo", c "bar!"]
  ]
