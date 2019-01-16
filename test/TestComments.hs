{-# LANGUAGE OverloadedStrings #-}
module TestComments
where

import Test.Tasty
import Test.Tasty.HUnit
import DelphiLexer
import Text.Megaparsec (parse)
import DelphiAst (Directive(..), Lexeme(..))

c a = Comment a
include a = Include (Lexeme Empty a)

commentTests :: TestTree
commentTests = testGroup "Comment Tests"
  [ testCase "//SimpleLineComment NewLine"
  $ (Right (Comment "SimpleLineComment") @=? )
  $ parse comment "" "//SimpleLineComment\n"
  , testCase "//SimpleLineComment"
  $ (Right (Comment "SimpleLineComment") @=? )
  $ parse comment "" "//SimpleLineComment"
  , testCase "//MultipleLineComment"
  $ (Right (Comment "MultipleLineComment\nSecondLine!") @=? )
  $ parse comment "" "//MultipleLineComment\n//SecondLine!"
  , testCase "//MultipleLineComment#2"
  $ (Right (Comment "MultipleLineComment\nSecondLine!") @=? )
  $ parse comment "" "//MultipleLineComment\n  //SecondLine!"
  , testCase "{SimpleBlockComment}NewLine"
  $ (Right (Comment "SimpleBlockComment") @=? )
  $ parse comment "" "{SimpleBlockComment}\n"
  , testCase "{SimpleBlockComment}"
  $ (Right (Comment "SimpleBlockComment") @=? )
  $ parse comment "" "{SimpleBlockComment}"
  , testCase "{SimpleBlockComment}//ThenLine"
  $ (Right (Comment "SimpleBlockComment\nThenLine") @=? )
  $ parse comment "" "{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  $ (Right (Comment "SimpleBlockComment\nSimpleBlockComment\nThenLine") @=? )
  $ parse comment "" "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment} \\n {SimpleBlockComment} //ThenLine"
  $ (Right (Comment "SimpleBlockComment\nSimpleBlockComment\nThenLine") @=? )
  $ parse comment "" "{SimpleBlockComment} {SimpleBlockComment} //ThenLine"
  , testCase "Multiple lines..."
  $ (Right (Comment "--\n This file starts with comments\n--\n And another comment") @=? )
  $ parse comment "" "{--\n This file starts with comments\n--}\n// And another comment\n\n\nunit TestUnit; interface type implementation initialization finalization end."
  , testCase "Nested Comments"
  $ (Right (Comment "hey{there}blah") @=? )
  $ parse comment "" "{hey{there}blah}"
  , testCase "Commented directive"
  $ (Right (Comment " $define blah") @=? )
  $ parse comment "" "{ $define blah}"
  , testCase "Directive: $i foo"
  $ (Right (include "foo") @=? )
  $ parse comment "" "{$i foo}"
  ]
