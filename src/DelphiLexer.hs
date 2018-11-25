{-# LANGUAGE OverloadedStrings #-}

module DelphiLexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , symbol'
  , parens
  , parens'
  , integer
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
import Data.Text (Text, strip, pack, unpack, words)

type Parser = Parsec Void String

-- For now, just skip comments.  But will later have to include them.
-- (because a code reformatter will need to preserve comments)
--
-- sc - the space consumer.
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "{" "}"  <|> L.skipBlockComment "(*" "*)"

-- Removes all spaces after a lexime.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol' :: String -> Parser String
symbol' = L.symbol sc

symbol :: Text -> Parser ()
symbol a = (\_ -> ()) <$> (symbol' $ unpack a)

parens :: Text -> Text -> Parser a -> Parser a
parens a b = between (symbol a) (symbol b)

parens' :: Char -> Char -> Parser a -> Parser a
parens' a b = parens (pack [a]) (pack [b])

integer :: Parser Integer
integer = lexeme L.decimal

hexinteger :: Parser Integer
hexinteger = (char '#' <|> char '$') *> lexeme L.hexadecimal

float :: Parser Rational
float = try $ do
  (a, c) <- lookAhead float_
  skipCount 1 float_
  return $ nom a c % den c
  where
    nom :: String -> String -> Integer
    nom a c = read $ a <> c
    den :: String -> Integer
    den c = toInteger $ (10 :: Integer) ^ (length c)
    float_ = do
      a <- some digitChar
      _ <- char '.'
      c <- some digitChar
      return (a, c)

semi :: Parser ()
semi = (\_ -> ()) <$> symbol ";"

reserved :: [Text]
reserved =
  words $
  "and array asm begin break case const constructor continue destructor div do downto else end false file for function goto if implementation in inline interface label mod nil not object of on operator or packed procedure program record repeat set shl shr string then to true type unit until uses var while with xor as class dispose except exit exports finalization finally inherited initialization is library new on out property raise threadvar try absolute abstract alias assembler cdecl cppdecl default export external forward generic index local nostackframe oldfpccall override pascal private protected public published read register reintroduce safecall softfloat specialize stdcall virtual write far near"

rword :: String -> Parser ()
rword w = (lexeme . try) (string' w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = unpack <$> identifier'

identifier' :: Parser Text
identifier' = identifierPlus []

identifierPlus :: [Text] -> Parser Text
identifierPlus override = strip <$> (lexeme . try) (p >>= check)
  where
    p :: Parser Text
    p = pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    check x =
      if x `elem` override
        then return x
        else if x `elem` reserved
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

anyIdentifier :: Parser String
anyIdentifier = (lexeme . try) $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
