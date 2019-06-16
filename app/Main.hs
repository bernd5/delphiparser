{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (putStrLn, intercalate)
import Data.List (foldl')
import DelphiParser
import DelphiAst
import System.Directory (getDirectoryContents, doesFileExist)
import Text.Megaparsec (runParser, ParseError(..))
import Data.Text.IO (putStrLn)
import Data.ByteString (hGetContents)
import System.IO (openBinaryFile, IOMode(ReadMode))
import Data.Text (pack, unpack, Text, intercalate)
import Data.Text.Encoding (decodeUtf8', decodeLatin1)
import Control.Monad (filterM, forM, forM_, when)
import qualified Control.Monad.Parallel as P (mapM)
import Control.Exception (handle, SomeException)
import System.FilePath ((</>), isExtensionOf)
import Control.Applicative ((<|>))
import Data.Maybe
import qualified Data.Map as Map
import Args
import TypeCategories

files :: FilePath -> IO [FilePath]
files d = do
  a <- doesFileExist d
  if a then
    pure [d]
  else do
    contents <- getDirectoryContents d
    let contents' = filter (\x -> x /= "." && x /= "..") contents
    r <- mapM (\x -> files (d </> x)) contents'
    pure $ concat r

getFileNames (Just dname) _ = files $ unpack dname
getFileNames _ (Just fname) = files $ unpack fname

parseFile x = handle onError $ do
    putStrLn . pack $ "Parsing file: " <> x
    h <- openBinaryFile x ReadMode
    bs <- hGetContents h
    let sp = case decodeUtf8' bs of
              Left _ -> decodeLatin1 bs
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


main = main' =<< getArgs

main' :: Args -> IO ()
main' args = do
  fileNames <- getFileNames (dir args) (file args)

  let pasfiles = filter (\x -> isExtensionOf "pas" x || isExtensionOf "pp" x) fileNames
  
  parsedFiles <- forM pasfiles parseFile

  let types = concatMap getTypes (concat parsedFiles)
  when (showTypes args) $ do
    putStrLn ""
    putStrLn . pack $ "Total files: " <> show (length parsedFiles)
    putStrLn . pack $ "Parsed:      " <> show (length $ filter (\x -> 0 /= length x) parsedFiles)
    putStrLn . pack $ "Total types: " <> show (length types)
    putStrLn ""
    let types' = categorizeTypes types
    forM_ (Map.keys types') ( \k -> do
      putStrLn $ "# " <> k
      forM_ (Map.findWithDefault [] k types') $ putStrLn . showType
      putStrLn ""
      )

showArgs :: forall a. (Show a) => [a] -> Text
showArgs args = (pack . show) args
showClassDef :: ClassDefinition -> Text
showClassDef def = (pack . show) def
showTypeName :: TypeName -> Text
showTypeName (Type (Lexeme _ name)) = name
showTypeName name = (pack . show) name
showTypeRhs :: TypeDefinitionRHS -> Text
showTypeRhs (ReferenceToProcedure args) = "procedure(" <> showArgs args <> ") -- reference"
showTypeRhs (SimpleProcedure args) = "procedure(" <> showArgs args <> ")"
showTypeRhs (ProcedureOfObject args) = "procedure(" <> showArgs args <> ") -- of object"
showTypeRhs (ReferenceToFunction args name) = "function " <> showTypeName name <> "(" <> showArgs args <> ") -- reference"
showTypeRhs (SimpleFunction args name) = "function " <> showTypeName name <> "(" <> showArgs args <> ")"
showTypeRhs (FunctionOfObject args name) = "function " <> showTypeName name <> "(" <> showArgs args <> ") -- of object"
showTypeRhs (NewType name) = "type " <> showTypeName name
showTypeRhs (ClassOf name) = "class of " <> showTypeName name
showTypeRhs (ClassHelper name classDef) = "class helper for " <> showTypeName name <> showClassDef classDef

showType :: TypeDefinition -> Text
showType (TypeDef name rhs) = intercalate " " [ name', rhs' ]
  where
    name' = showTypeName name
    rhs' = showTypeRhs rhs
showType (TypeAlias name newName) = "alias " <> name' <> " -> " <> newName'
  where
    name' = showTypeName name
    newName' = showTypeName newName
showType (EnumDefinition name items) = "enum " <> name' <> " = (" <> items' <> ")"
  where
    name' = showTypeName name
    items' :: Text
    items' = intercalate ", " $ foldl' (\b (Lexeme _ a) -> b <> [a]) [] items
showType a = (pack . show) a


onError :: SomeException -> IO [Unit]
onError e = do
  putStrLn . pack $ show e
  return []

getTypes :: Unit -> [TypeDefinition]
getTypes (Unit _ _ (Interface _ c) _ _ _) = getTypesIE c
getTypes _ = []

getTypesIE :: [InterfaceExpression] -> [TypeDefinition]
getTypesIE (TypeDefinitions x:xs) = x <> getTypesIE xs
getTypesIE (_:xs) = getTypesIE xs
getTypesIE _ = []
