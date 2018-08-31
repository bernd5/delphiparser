module Main where

import Prelude hiding (putStrLn)
import DelphiParser
import DelphiWriter
import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import Data.Text.IO (putStrLn)
import Data.Char (ord)

main :: IO ()
main = do
  args <- getArgs
  let input = (head args)
  sp <- readFile (head args)
  let p = runParser dUnitP input sp
  case p of
    Left a -> fail $ show a
    Right a -> putStrLn $ showDelphi a
