{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  ) where

import Prelude hiding (putStrLn)
import DelphiParser
import DelphiWriter
import Data.Either (either)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import Text.Megaparsec (parseTest', runParser)
import Data.Text.IO (putStrLn)

someFunc :: IO ()
someFunc
 = do
  args <- getArgs
  let input = (head args)
  sp <- readFile (head args)
  let p = runParser dUnitP input sp
  case p of
    Left a -> fail $ show a
    Right a -> putStrLn $ showDelphi a
