{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (putStrLn)
import DelphiParser
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesFileExist)
import Text.Megaparsec (runParser, ParseError(..))
import Data.Text.IO (putStrLn, hGetContents)
import System.IO (hSetEncoding, localeEncoding, openBinaryFile, IOMode(ReadMode))
import Data.Text (unpack, pack)
import Control.Monad (filterM)
import Control.Exception (handle, SomeException)
import System.FilePath ((</>), isExtensionOf)
import Control.Applicative ((<|>))
import DelphiWriter

main :: IO ()
main = do
  args <- getArgs
  let dirname = (head args)

  contents <- getDirectoryContents dirname
  let contents' = map (dirname </>) contents
  putStrLn . pack  $ show contents
  let
    files :: IO [FilePath]
    files = filterM doesFileExist contents'

  files' <- files
  let pasfiles = filter (isExtensionOf "pas") files'
  
  r <- flip mapM pasfiles $ \x -> handle onError $ do
    putStrLn . pack $ "Parsing file: " <> x
    h <- openBinaryFile x ReadMode
    hSetEncoding h localeEncoding
    sp <- hGetContents h
    let p = runParser (dUnitP <|> program) x (unpack sp)
    case p of
      Left a -> case a of
        TrivialError o (Just e) s -> do
          putStrLn . pack $ "O: " <> show o
          return False
        otherwise -> do
          putStrLn . pack $ "A: " <> show a
          return False
      Right !a -> do
        --putStrLn $ showDelphi a
        return True


  putStrLn . pack $ "Total types: " <> show (length r)
  putStrLn . pack $ "Total files: " <> show (length r)
  putStrLn . pack $ "Parsed:      " <> show (length $ filter id r)

onError :: SomeException -> IO Bool
onError e = do
  putStrLn . pack $ show e
  return False

