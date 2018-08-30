{-# LANGUAGE OverloadedStrings #-}

module TestLoops ( loopTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (loop')

import DelphiAst

loopTests :: TestTree
loopTests = testGroup "Delphi Loop Tests"
  [ testCase "for I := foo to bar do begin end"  $
    (Right (For (V "I" := V "foo") LoopUpTo (V "bar") (Begin [])) @=?) $
    parse loop' "" "for I := foo to bar do begin end"
  , testCase "for I := foo downto bar do begin end"  $
    (Right (For (V "I" := V "foo") LoopDownTo (V "bar") (Begin [])) @=?) $
    parse loop' "" "for I := foo downto bar do begin end"
  , testCase "while foo <= bar do begin end" $
    (Right (While (V "foo" :<= V "bar") (Begin [])) @=? ) $
    parse loop' "" "while foo <= bar do begin end"
  , testCase "repeat ... until foo <= bar" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse loop' "" "repeat A := B; B := A until foo <= bar"
  ]
