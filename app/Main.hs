module Main where

import Prelude hiding (putStrLn)
import DelphiParser
import DelphiWriter
import System.Environment (getArgs)
import Text.Megaparsec (runParser)
import Data.Text.IO (putStrLn, hGetContents)
import System.IO (hSetEncoding, localeEncoding, openBinaryFile, IOMode(ReadMode))
import Data.Text (unpack)

main :: IO ()
main = do
  args <- getArgs
  let input = (head args)
  h <- openBinaryFile (head args) ReadMode
  hSetEncoding h localeEncoding
  sp <- hGetContents h
  let p = runParser dUnitP input (unpack sp)
  case p of
    Left a -> fail $ show a
    Right a -> putStrLn $ showDelphi a
