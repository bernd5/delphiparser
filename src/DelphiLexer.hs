{-# LANGUAGE OverloadedStrings #-}

module DelphiLexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , end
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

import Prelude hiding (words, length, concat)
import qualified Prelude as P
import Data.Void
import Data.Ratio ((%))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, toLower, strip, pack, unpack, words, intercalate, length, singleton, concat, breakOn, breakOnAll)
import Control.Applicative (empty)
import Control.Monad (void, replicateM)
import Data.Maybe (fromMaybe, isJust)
import DelphiAst (Lexeme(..), Directive(..))

type Parser = Parsec Void Text

-- For now, just skip comments.  But will later have to include them.
-- (because a code reformatter will need to preserve comments)
--
-- sc - the space consumer.
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = empty
    blockCmnt = empty

takeWhileP' :: Maybe String -> (Token Text -> Bool) -> Parser Text
takeWhileP' = takeWhileP

-- TODO: Make this take a continuation in the event that it's an include or if or something?
-- And have it return not a `Parser Directive`, but the 'new' parse.
comment :: Parser Directive
comment = do
  a <- optional $ many $ choice [ try lineComment
                 , try blockComment
                 ]
  return $ foldr (<>) Empty $ fromMaybe [] a

lineComment :: Parser Directive
lineComment = do
  a <- some ((string "//" *> takeWhileP' (Just "character") (/= '\n')) <* space)
  return $ Comment $ intercalate "\n" a

blockComment' :: Text -> Text -> Parser Directive
blockComment' a b = do
  string a
  if a == "{" then
    choice [ try compilerDirective
           , restOfBlockComment' a b
           ] <* space
  else
    (restOfBlockComment' a b) <* space

compilerDirective :: Parser Directive
compilerDirective = do
  char '$'
  directive <- takeWhileP' (Just "character") (/= '}') <* char '}'
  let
    directive' :: (Text, Text)
    directive' = breakOn " " directive
  case directive' of
    ("i", b) -> do
      return $ Include (Lexeme Empty (strip b))
    ("if", b) -> do
      a <- readToEndDirective "endif"
      case a of
        (Lexeme _ "else") -> do
          e <- readToEndDirective "endif"
          return $ IfDef b a e
        _ -> do
          return $ IfDef b a (Lexeme Empty "")
    _ -> return $ Comment $ "$" <> directive

readToEndDirective :: Text -> Parser (Lexeme Text)
readToEndDirective _ = do
  a <- lexeme (takeWhileP' (Just "character") (/= '{'))

  return $ a

restOfBlockComment' :: Text -> Text -> Parser Directive
restOfBlockComment' a b = do
  c <- pack <$> (manyTill anyChar (string b))
  let a' = P.length $ breakOnAll a c
  let b' = P.length $ breakOnAll b c
  c' <- replicateM (a' - b') (pack <$> (manyTill anyChar (string b)))

  return $ Comment $ intercalate b ([c] <> c')

blockComment :: Parser Directive
blockComment = do
  c <- some $ (choice
    [ blockComment' "{" "}"
    , blockComment' "(*" "*)"
    ] <* space )
  let c' = foldr (<>) Empty c
  d <- optional lineComment
  let d' = fromMaybe Empty d
  return $ c' <> d'

-- Removes all spaces after a lexime.
lexeme :: Parser a -> Parser (Lexeme a)
lexeme a = do
  b <- L.lexeme sc a
  c <- comment
  return $ Lexeme c b

symbol' :: Text -> Parser (Lexeme Text)
symbol' a = do
  b <- L.symbol sc a
  c <- comment
  return $ Lexeme c b

symbol :: Text -> Parser ()
symbol a = void $ symbol' a

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
    nom :: Text -> Text -> Integer
    nom a c = read . unpack $ a <> c
    den :: Text -> Integer
    den c = toInteger $ (10 :: Integer) ^ length c
    float_ :: Parser (Text, Text)
    float_ = do
      a <- some (singleton <$> digitChar)
      _ <- char '.'
      c <- some (singleton <$> digitChar)
      return (concat a, concat c)

semi :: Parser ()
semi = void $ symbol ";"

reserved :: [Text]
reserved =
  words
    "and array asm begin break case const constructor continue destructor div do downto else end false file for function goto if implementation in inline interface label mod nil not object of on operator or packed procedure program record repeat set shl shr string then to true type unit until uses var while with xor as class dispose except exit exports finalization finally inherited initialization is library new on out property raise threadvar try absolute abstract alias assembler cdecl cppdecl default export external forward generic index local nostackframe oldfpccall override pascal private protected public published read register reintroduce safecall softfloat specialize stdcall virtual write far near"

rword :: Text -> Parser (Lexeme ())
rword w = (lexeme . try) (string' w *> notFollowedBy alphaNumChar)

end :: Parser (Lexeme ())
end = rword "end"

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
