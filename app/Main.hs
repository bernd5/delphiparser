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
import System.IO (openBinaryFile, IOMode(ReadMode))
import Data.Text (pack, Text, intercalate)
import Data.Text.Encoding (decodeUtf8', decodeLatin1)
import Control.Exception (handle, SomeException)
import System.FilePath ((</>), isExtensionOf)
import Control.Applicative ((<|>))

genUnitDots :: [(FilePath, Maybe Unit)] -> Text
genUnitDots files = intercalate "\n" $ do
  (a, b) <- files
  case b of
    Just b' -> return $ intercalate
      "\n"
      (map (\x -> intercalate " -> " [(unitName b'), x]) (units b'))
    Nothing -> return $ "-- " <> (pack a) <> " was not parsed"

unitName :: Unit -> Text
unitName (Unit _ (Lexeme _ a) _ _ _ _) = a
unitName (Program      (Lexeme _ a) _) = a
unitName (UnitFragment _            _) = "<fragment>"

units :: Unit -> [Text]
units (Unit _ _ (Interface (Uses a) _) (Implementation (Uses b) _) _ _) = map f
  $ concat (a <> b)
 where
  f :: Lexeme Text -> Text
  f (Lexeme _ c) = c
units _ = []


files :: FilePath -> IO [FilePath]
files d = do
  a <- doesFileExist d
  if a
    then pure $ [d]
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

  let pasfiles =
        filter (\x -> isExtensionOf "pas" x || isExtensionOf "pp" x) contents
  putStrLn . pack $ show pasfiles

  r <- flip mapM pasfiles $ \x -> (handle (onError x) (mainParseFile x))

  putStrLn . pack $ "Total files: " <> show (length r)

  putStrLn $ genUnitDots r

onError :: FilePath -> SomeException -> IO (FilePath, Maybe Unit)
onError x e = do
  putStrLn . pack $ show e
  return (x, Nothing)

mainParseFile :: FilePath -> IO (FilePath, Maybe Unit)
mainParseFile x = do
  putStrLn . pack $ "Parsing file: " <> x
  h  <- openBinaryFile x ReadMode
  bs <- hGetContents h
  let sp = case decodeUtf8' bs of
        Left  _ -> decodeLatin1 bs
        Right t -> t
  let p = runParser pascalFile x sp
  case p of
    Left a -> case a of
      TrivialError o (Just e) s -> do
        putStrLn . pack $ "O: " <> show o
        putStrLn . pack $ "E: " <> show e
        putStrLn . pack $ "S: " <> show s
        return (x, Nothing)
      _ -> do
        putStrLn . pack $ "A: " <> show a
        return (x, Nothing)
    Right !a -> do
      --putStrLn $ showDelphi a
      return (x, Just a)
