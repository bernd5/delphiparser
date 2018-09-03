{-# LANGUAGE OverloadedStrings #-}

module TestLoops ( loopTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (loop', statement)

import Data.List (intercalate)
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
  , testCase "repeat ...; until foo <= bar" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse loop' "" "repeat A := B; B := A; until foo <= bar"
  , testCase "(statement) repeat ...; until foo <= bar" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse statement "" "repeat A := B; B := A; until foo <= bar"
  , testCase "repeat begin ... end; until foo <= bar" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse loop' "" "repeat begin end; until foo <= bar"
  , testCase "Nested repeat ...; until foo <= bar" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse loop' "" "repeat repeat A := B; until foo <= bar; until foo <= bar"
  , testCase "Nested repeat ...; until foo <= bar inside begin/end" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse loop' "" "repeat begin repeat A := B; until foo <= bar; end; until foo <= bar"
  , testCase "(statement) Begin repeat end;" $
    (Right (Repeat [V "A" := V "B",V "B" := V "A"] (V "foo" :<= V "bar")) @=? ) $
    parse statement "" "begin repeat A := B; until foo <= bar; end"
  ]
