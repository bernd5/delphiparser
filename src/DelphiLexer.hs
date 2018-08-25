module DelphiLexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , parens
  , integer
  , semi
  , rword
  , anyIdentifier
  , identifier
  , identifier'
  ) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Data.Text (Text, strip, pack, unpack)

type Parser = Parsec Void String

-- For now, just skip comments.  But will later have to include them.
-- (because a code reformatter will need to preserve comments)
--
-- sc - the space consumer.
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "{" "}"

-- Removes all spaces after a lexime.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc
symbol' a = symbol $ unpack a

parens :: Text -> Text -> Parser a -> Parser a
parens a b = between (symbol' a) (symbol' b)

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

reserved =
  words $
  "and array asm begin break case const constructor continue destructor div do downto else end false file for function goto if implementation in inline interface label mod nil not object of on operator or packed procedure program record repeat set shl shr string then to true type unit until uses var while with xor as class dispose except exit exports finalization finally inherited initialization is library new on out property raise self threadvar try absolute abstract alias assembler cdecl cppdecl default export external forward generic index local name nostackframe oldfpccall override pascal private protected public published read register reintroduce safecall softfloat specialize stdcall virtual write far near"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = unpack <$> identifier'

identifier' :: Parser Text
identifier' = strip . pack <$> (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reserved
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

anyIdentifier :: Parser String
anyIdentifier = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar
