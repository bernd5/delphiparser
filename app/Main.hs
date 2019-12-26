{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Main where

import           Prelude                 hiding ( putStrLn )
import           DelphiParser
import           DelphiAst
import           System.Directory               ( getDirectoryContents
                                                , doesFileExist
                                                )
import           Text.Megaparsec                ( runParser
                                                , ParseError(..)
                                                )
import           Data.Text.IO                   ( putStrLn )
import           Data.ByteString                ( hGetContents )
import           System.IO                      ( openBinaryFile
                                                , IOMode(ReadMode)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Encoding             ( decodeUtf8'
                                                , decodeLatin1
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                , when
                                                )
import           Control.Exception              ( handle
                                                , SomeException
                                                )
import           System.FilePath                ( (</>)
                                                , isExtensionOf
                                                )
import qualified Data.Map                      as Map
import qualified Args
import           TypeCategories
import           AstPrettyPrint
import           HaskellPrettyPrint
import Web
import Graphs

unitName :: Unit -> Text
unitName (Unit _ (Lexeme _ a) _ _ _ _) = a
unitName (Program      (Lexeme _ a) _ _ _) = a
unitName (UnitFragment _            _) = "<fragment>"

units :: Unit -> [Text]
units (Unit _ _ (Interface (Uses a _) _) (Implementation (Uses b _) _) _ _) = map f
  $ a <> b
 where
  f :: Lexeme Text -> Text
  f (Lexeme _ c) = c
units _ = []


files :: FilePath -> IO [FilePath]
files d = do
  a <- doesFileExist d
  if a
    then pure [d]
    else do
      contents <- getDirectoryContents d
      let contents' = filter (\x -> x /= "." && x /= "..") contents
      r <- mapM (\x -> files (d </> x)) contents'
      pure $ concat r

getFileNames :: Maybe Text -> Maybe Text -> IO [FilePath]
getFileNames (Just dname) _            = files $ unpack dname
getFileNames _            (Just fname) = files $ unpack fname
getFileNames _            _            = pure []

parseFile :: String -> IO [Unit]
parseFile x = handle onError $ do
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
        return []
      _ -> do
        putStrLn . pack $ "A: " <> show a
        return []
    Right !a -> return [a]

main :: IO ()
main = main' =<< Args.getArgs

main' :: Args.Args -> IO ()
main' args = do
  fileNames <- getFileNames (Args.dir args) (Args.file args)

  let pasfiles =
        filter (\x -> isExtensionOf "pas" x || isExtensionOf "pp" x) fileNames

  parsedFiles <- forM pasfiles parseFile :: IO [[Unit]]

  let types = concatMap getTypes (concat parsedFiles)
  when (Args.showTypes args) $ do
    putStrLn ""
    putStrLn . pack $ "Total files: " <> show (length parsedFiles)
    putStrLn . pack $ "Parsed:      " <> show
      (length $ filter (\x -> 0 /= length x) parsedFiles)
    putStrLn . pack $ "Total types: " <> show (length types)
    putStrLn ""
    let types' = categorizeTypes types
    forM_
      (Map.keys types')
      (\k -> do
        putStrLn $ "# " <> k
        forM_ (Map.findWithDefault [] k types') $ putStrLn . pp
        putStrLn ""
      )


  -- putStrLn $ pack $ show parsedFiles

  let parsedFiles' = foldr (<>) [] parsedFiles
  when (Args.saveDot args) $ do
    generateUnitReferencesDots "units.dot" $ zip fileNames parsedFiles'

  when (Args.reformatPascal args) $ do
    forM_ parsedFiles' $ \unit -> do
      putStrLn $ pp unit
  when (Args.reformatHaskell args) $ do
    forM_ parsedFiles' $ \unit -> do
      putStrLn $ hh unit

  case Args.docWeb args of
    Just port -> serveWeb port pasfiles parsedFiles'
    Nothing   -> pure ()

onError :: SomeException -> IO [Unit]
onError e = do
  putStrLn . pack $ show e
  return []

getTypes :: Unit -> [TypeDefinition]
getTypes (Unit _ _ (Interface _ c) _ _ _) = getTypesIE c
getTypes (Program _ _ c _) = concatMap getTypesIE' c
  where
    getTypesIE' (AdditionalInterface a) = getTypesIE [a]
    getTypesIE' _ = []
getTypes _ = []

getTypesIE :: [InterfaceExpression] -> [TypeDefinition]
getTypesIE (TypeDefinitions x : xs) = x <> getTypesIE xs
getTypesIE (_                 : xs) = getTypesIE xs
getTypesIE _                        = []
