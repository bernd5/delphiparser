{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Data.Text
import DelphiAst
import DelphiLexer
import DelphiParser
import Text.Pretty.Simple (pPrint)

someFunc :: IO ()
someFunc
 = do
  putStrLn "Please see tests"
