{-# LANGUAGE OverloadedStrings #-}

module TestLoops ( loopTests ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Text.Megaparsec (parse)
import DelphiParser (loop', statement, expression')

import DelphiAst

v a = V $ Lexeme Empty a
s a = S $ Lexeme Empty a

loopTests :: TestTree
loopTests = testGroup "Delphi Loop Tests"
  [ testCase "for I := foo to bar do begin end"  $
    (Right (For (v "I" := v "foo") LoopUpTo (v "bar") (Begin [])) @=?) $
    parse loop' "" "for I := foo to bar do begin end"
  , testCase "for I := foo downto bar do begin end"  $
    (Right (For (v "I" := v "foo") LoopDownTo (v "bar") (Begin [])) @=?) $
    parse loop' "" "for I := foo downto bar do begin end"
  , testCase "while foo <= bar do begin end" $
    (Right (While (v "foo" :<= v "bar") (Begin [])) @=? ) $
    parse loop' "" "while foo <= bar do begin end"
  , testCase "while (foo <= bar) and (c[i]<=length('\\n')) do begin ... end" $
    (Right
      (While
        (P [v "foo" :<= v "bar"] :& P [v "c" :!! [v "i"] :<= ((v "length") :$ [s "\\n"])])
          (Begin [ v "result" := ((v "result") :+ ((v "s") :!! [v "i"]))
                 , ExpressionValue $ (v "inc") :$ [v "i"]
                 ]
          )) @=? ) $
    parse loop' "" "while (foo <= bar) and (c[i]<=length('\\n')) do begin result:=result+s[i]; inc(i); end"
  , testCase "result+s[i]" $
    (Right (v "result" :+ (v "s" :!! [v "i"])) @=? ) $
    parse expression' "" "result+s[i]"
  , testCase "result:=result+s[i] // Statement" $
    (Right (v "result" := (v "result" :+ (v "s" :!! [v "i"]))) @=? ) $
    parse statement "" "result:=result+s[i]"
  , testCase "begin result:=result+s[i]; end" $
    (Right (Begin [v "result" := (v "result" :+ (v "s" :!! [v "i"]))]) @=? ) $
    parse statement "" "begin result:=result+s[i]; end"
  , testCase "begin result:=result+s[i]; inc(i); end" $
    (Right (Begin
      [ v "result" := (v "result" :+ (v "s" :!! [v "i"]))
      , ExpressionValue (v "inc" :$ [v "i"])]) @=? ) $
    parse statement "" "begin result:=result+s[i]; inc(i); end"
  , testCase "(foo <= bar) and (c[i]<=length('\\n'))" $
    (Right (P [v "foo" :<= v "bar"] :& P [(v "c" :!! [v "i"]) :<= (v "length" :$ [s "\\n"])]) @=? ) $
    parse expression' "" "(foo <= bar) and (c[i]<=length('\\n'))"
  , testCase "while (foo <= bar) and (c[i] <> '\\n') do begin end" $
    (Right (While (P [v "foo" :<= v "bar"] :& P [(v "c" :!! [v "i"]) :<> s "\\n"]) (Begin [])) @=? ) $
    parse loop' "" "while (foo <= bar) and (c[i] <> '\\n') do begin end"
  , testCase "repeat ...; until foo <= bar" $
    (Right (Repeat [v "A" := v "B",v "B" := v "A"] (v "foo" :<= v "bar")) @=? ) $
    parse loop' "" "repeat A := B; B := A; until foo <= bar"
  , testCase "(statement) repeat ...; until foo <= bar" $
    (Right (Repeat [v "A" := v "B",v "B" := v "A"] (v "foo" :<= v "bar")) @=? ) $
    parse statement "" "repeat A := B; B := A; until foo <= bar"
  , testCase "repeat begin end; until foo <= bar" $
    (Right (Repeat [Begin []] (v "foo" :<= v "bar")) @=? ) $
    parse loop' "" "repeat begin end; until foo <= bar"
  , testCase "Nested repeat ...; until foo <= bar" $
    (Right (Repeat [Repeat [v "A" := v "B"] (v "foo" :<= v "bar")] (v "foo" :<= v "bar")) @=? ) $
    parse loop' "" "repeat repeat A := B; until foo <= bar; until foo <= bar"
  , testCase "Nested repeat ...; until foo <= bar inside begin/end" $
    (Right (Repeat [Begin [Repeat [v "A" := v "B"] (v "foo" :<= v "bar")]] (v "foo" :<= v "bar")) @=? ) $
    parse loop' "" "repeat begin repeat A := B; until foo <= bar; end; until foo <= bar"
  , testCase "(statement) Begin repeat end;" $
    (Right (Begin [Repeat [v "A" := v "B"] (v "foo" :<= v "bar")]) @=? ) $
    parse statement "" "begin repeat A := B; until foo <= bar; end"
  ]
