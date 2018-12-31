{-# LANGUAGE OverloadedStrings #-}

module DelphiLexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , reserved
  , symbol'
  , parens
  , parens'
  , integer
  , comment
  , hexinteger
  , float
  , semi
  , rword
  , anyIdentifier
  , identifier
  , identifier'
  , identifierPlus
  ) where

import Prelude hiding (words)
import Data.Void
import Data.Ratio ((%))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, toLower, strip, pack, unpack, words, intercalate)
import Control.Applicative (empty)
import Control.Monad (void)
import Data.Maybe (fromMaybe, isJust)
import DelphiAst (Lexeme(..))

type Parser = Parsec Void String

-- For now, just skip comments.  But will later have to include them.
-- (because a code reformatter will need to preserve comments)
--
-- sc - the space consumer.
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = empty
    blockCmnt = empty

comment :: Parser Text
comment = do
  a <- optional $ many $ choice [ try lineComment
                 , try blockComment
                 ]
  return $ intercalate "\n" $ fromMaybe [] a

lineComment :: Parser Text
lineComment = do
  a <- some ((string "//" *> takeWhileP (Just "character") (/= '\n')) <* space)
  return $ intercalate "\n" (pack <$> a)

blockComment :: Parser Text
blockComment = do
  c <- some $ (choice
    [ char '{' >> (manyTill anyChar (char '}'))
    , string "(*" >> (manyTill anyChar (string "*)"))
    ] <* space )
  let c' = (intercalate "\n") $ map pack $ c
  d <- optional lineComment
  let d' = fromMaybe "" d
  return $ intercalate "\n" $ filter (\x -> x /= "") [c', d']

-- Removes all spaces after a lexime.
lexeme :: Parser a -> Parser (Lexeme a)
lexeme a = do
  b <- L.lexeme sc a
  c <- comment
  return $ Lexeme c b

symbol' :: String -> Parser (Lexeme Text)
symbol' a = do
  b <- pack <$> L.symbol sc a
  c <- comment
  return $ Lexeme c b

symbol :: Text -> Parser ()
symbol a = void $ symbol' (unpack a)

parens :: Text -> Text -> Parser a -> Parser a
parens a b = between (symbol a) (symbol b)

parens' :: Char -> Char -> Parser a -> Parser a
parens' a b = parens (pack [a]) (pack [b])

integer :: Parser (Lexeme Integer)
integer = lexeme L.decimal

hexinteger :: Parser (Lexeme Integer)
hexinteger = char '$' *> lexeme L.hexadecimal

float :: Parser Rational
float = try $ do
  (a, c) <- lookAhead float_
  skipCount 1 float_
  return $ nom a c % den c
  where
    nom :: String -> String -> Integer
    nom a c = read $ a <> c
    den :: String -> Integer
    den c = toInteger $ (10 :: Integer) ^ length c
    float_ = do
      a <- some digitChar
      _ <- char '.'
      c <- some digitChar
      return (a, c)

semi :: Parser ()
semi = void $ symbol ";"

reserved :: [Text]
reserved =
  words
    "and array asm begin break case const constructor continue destructor div do downto else end false file for function goto if implementation in inline interface label mod nil not object of on operator or packed procedure program record repeat set shl shr string then to true type unit until uses var while with xor as class dispose except exit exports finalization finally inherited initialization is library new on out property raise threadvar try absolute abstract alias assembler cdecl cppdecl default export external forward generic index local nostackframe oldfpccall override pascal private protected public published read register reintroduce safecall softfloat specialize stdcall virtual write far near"

rword :: String -> Parser (Lexeme ())
rword w = (lexeme . try) (string' w *> notFollowedBy alphaNumChar)

identifier :: Parser (Lexeme String)
identifier = do
  a <- identifier'
  return $ unpack <$> a

identifier' :: Parser (Lexeme Text)
identifier' = identifierPlus []

identifierPlus :: [Text] -> Parser (Lexeme Text)
identifierPlus override = (\x -> strip <$> x)  <$> (lexeme . try) (p >>= check)
  where
    p :: Parser Text
    p = pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    check x
      | (toLower x) `elem` override = return x
      | (toLower x) `elem` reserved =
        fail $ "keyword " ++ show x ++ " cannot be an identifier"
      | otherwise = return x

anyIdentifier :: Parser (Lexeme String)
anyIdentifier = (lexeme . try) $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
