module Test where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Visibility = Visibility String [String]
  deriving Show

semi = symbol ";"

reserved = ["public", "private"]

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "{" "}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reserved
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

anyIdentifier :: Parser String
anyIdentifier = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar

testVisibilityP :: Parser Visibility
testVisibilityP = do
  accessibility <- anyIdentifier
  fields <- sepBy1' identifier semi
  return $ Visibility accessibility fields

sepBy1' p s = p >>= \x -> many $ (try s) >> p

testP = sepBy1' testVisibilityP semi >> symbol "."

tryParse = parseTest' testP "public one; two; private one; two;."
