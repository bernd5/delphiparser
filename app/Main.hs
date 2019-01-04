{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn)
import DelphiParser
import DelphiAst
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesFileExist)
import Text.Megaparsec (runParser, ParseError(..))
import Data.Text.IO (putStrLn)
import Data.ByteString (hGetContents)
import System.IO (hSetEncoding, latin1, localeEncoding, openBinaryFile, IOMode(ReadMode))
import Data.Text (unpack, pack, intercalate)
import Data.Text.Encoding (decodeUtf8', decodeLatin1)
import Control.Monad (filterM)
import qualified Control.Monad.Parallel as P (mapM)
import Control.Exception (handle, SomeException)
import System.FilePath ((</>), isExtensionOf)
import Control.Applicative ((<|>))
import DelphiWriter

files :: FilePath -> IO [FilePath]
files d = do
  a <- doesFileExist d
  if a then
    pure $ [d]
  else do
    contents <- getDirectoryContents d
    let contents' = filter (\x -> x /= "." && x /= "..") contents
    r <- mapM (\x -> files (d </> x)) contents'
    pure $ concat $ r

main :: IO ()
main = do
  args <- getArgs
  let dirname = (head args)

  contents <- files dirname
  putStrLn . pack  $ show contents

  let pasfiles = filter (\x -> isExtensionOf "pas" x || isExtensionOf "pp" x) contents
  
  r <- flip mapM pasfiles $ \x -> handle onError $ do
    putStrLn . pack $ "Parsing file: " <> x
    h <- openBinaryFile x ReadMode
    bs <- hGetContents h
    let sp = case decodeUtf8' bs of
              Left _ -> decodeLatin1 bs
              Right t -> t
    let p = runParser (dUnitP <|> program) x (unpack sp)
    case p of
      Left a -> case a of
        TrivialError o (Just e) s -> do
          putStrLn . pack $ "O: " <> show o
          putStrLn . pack $ "E: " <> show e
          putStrLn . pack $ "S: " <> show s
          return []
        otherwise -> do
          putStrLn . pack $ "A: " <> show a
          return []
      Right !a -> do
        --putStrLn $ showDelphi a
        return [a]

  let types = concat $ map getTypes $ concat r
  --putStrLn $ intercalate "\n=======\n" $ map showDelphi types

  putStrLn . pack $ "Total files: " <> show (length r)
  putStrLn . pack $ "Parsed:      " <> show (length $ filter (\x -> 0 /= length x) r)
  putStrLn . pack $ "Total types: " <> show (length types)

onError :: SomeException -> IO [Unit]
onError e = do
  putStrLn . pack $ show e
  return []

getTypes :: Unit -> [TypeDefinition]
getTypes (Unit _ _ (Interface _ c) impl i f) = getTypesIE c
getTypes a = []

getTypesIE :: [InterfaceExpression] -> [TypeDefinition]
getTypesIE ((TypeDefinitions x):xs) = (x <> getTypesIE xs)
getTypesIE (_:xs) = getTypesIE xs
getTypesIE _ = []
