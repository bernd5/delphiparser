{-# LANGUAGE OverloadedStrings #-}
module TestComments
where

import Test.Tasty
import Test.Tasty.HUnit
import DelphiLexer
import Text.Megaparsec (parse)

commentTests :: TestTree
commentTests = testGroup "Comment Tests"
  [ testCase "//SimpleLineComment NewLine"
  $ (Right "SimpleLineComment" @=? )
  $ parse comment "" "//SimpleLineComment\n"
  , testCase "//SimpleLineComment"
  $ (Right "SimpleLineComment" @=? )
  $ parse comment "" "//SimpleLineComment"
  , testCase "//MultipleLineComment"
  $ (Right "MultipleLineComment\nSecondLine!" @=? )
  $ parse comment "" "//MultipleLineComment\n//SecondLine!"
  , testCase "//MultipleLineComment#2"
  $ (Right "MultipleLineComment\nSecondLine!" @=? )
  $ parse comment "" "//MultipleLineComment\n  //SecondLine!"
  , testCase "{SimpleBlockComment}NewLine"
  $ (Right "SimpleBlockComment" @=? )
  $ parse comment "" "{SimpleBlockComment}\n"
  , testCase "{SimpleBlockComment}"
  $ (Right "SimpleBlockComment" @=? )
  $ parse comment "" "{SimpleBlockComment}"
  , testCase "{SimpleBlockComment}//ThenLine"
  $ (Right "SimpleBlockComment\nThenLine" @=? )
  $ parse comment "" "{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  $ (Right "SimpleBlockComment\nSimpleBlockComment\nThenLine" @=? )
  $ parse comment "" "{SimpleBlockComment}{SimpleBlockComment}//ThenLine"
  , testCase "{SimpleBlockComment} \n {SimpleBlockComment} //ThenLine"
  $ (Right "SimpleBlockComment\nSimpleBlockComment\nThenLine" @=? )
  $ parse comment "" "{SimpleBlockComment} {SimpleBlockComment} //ThenLine"
  , testCase "Multiple lines..."
  $ (Right "--\n This file starts with comments\n--\n And another comment" @=? )
  $ parse comment "" "{--\n This file starts with comments\n--}\n// And another comment\n\n\nunit TestUnit; interface type implementation initialization finalization end."
  ]
