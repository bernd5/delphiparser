{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

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
  , semi'
  , rword
  , compilerDirective
  , anyIdentifier
  , identifier
  , identifier'
  , identifierPlus
  ) where

import Prelude hiding (words, length, concat, take, count)
import qualified Prelude as P
import Data.Char (isAlphaNum)
import Data.Void
import Data.Ratio ((%))
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, toLower, strip, pack, unpack, words, intercalate, length, singleton, concat, breakOn, breakOnAll, take, isPrefixOf, count)
import Control.Applicative (empty)
import Control.Monad (void, replicateM)
import Data.Maybe (fromMaybe, isJust)
import DelphiAst (Lexeme(..), Directive(..))

type Parser = Parsec Void Text

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
  a <- many $ do
    choice [ lineComment
           , blockComment' "{" "}"
           , blockComment' "(*" "*)"]

  pure $ mconcat $ removeEmpties' a

blockComment :: Parser Directive
blockComment = do
  a <- some $ do
    choice [ blockComment' "{" "}"
           , blockComment' "(*" "*)"]

  pure $ mconcat $ removeEmpties' a

lineComment :: Parser Directive
lineComment = do
  a <- some ((string "//" *> takeWhileP' (Just "character") (/= '\n')) <* space)
  return $ Comment $ intercalate "\n" a

blockComment' :: Text -> Text -> Parser Directive
blockComment' "{" "}" = do
  char '{'
  choice [ compilerDirective
         , restOfBlockComment' "{" "}"
         ] <* space

blockComment' a b = do
  string a
  (restOfBlockComment' a b) <* space

readUpToEndComment :: Int -> Parser Text
readUpToEndComment 0 = ""
readUpToEndComment a = do
  lhs <- takeWhileP' (Just "character") (/= '}') <* char '}'
  let nesting = (count "{" lhs)
  rhs <- readUpToEndComment $ a - 1 + nesting
  return $ lhs <> (if nesting > 0 then "}" else "") <> rhs

compilerDirective :: Parser Directive
compilerDirective = do
  char '$'
  directive <- readUpToEndComment 1
  let
    directive' :: (Text, Text)
    directive' = breakOn " " directive
  case directive' of
    ("i", b) -> do
      return $ Include (strip b)
    ("if", cond) -> do
      let cond' = strip cond
      a <- takeWhileP' (Just "character") (/= '{') <* char '{'
      restOfIf cond' [Right a] []
    ("ifdef", cond) -> do
      let cond' = strip cond
      a <- takeWhileP' (Just "character") (/= '{') <* char '{'
      restOfIf cond' [Right a] []
    (a', b) -> pure $ UnknownDirective (a', b)

restOfIf
  :: Text
  -> [Either Directive Text]
  -> [Either Directive Text]
  -> Parser Directive
restOfIf cond a b = choice [ try $ compilerDirective >>= processIfDirectivePart cond a b
                         , do
                            a' <- Left <$> Comment <$> readUpToEndComment 1
                            b' <- Right <$> takeWhileP' (Just "character") (/= '{') <* char '{'
                            let a'' = removeEmpties (a <> [a', b'])
                            restOfIf cond a'' b
                         ]

restOfIfElse
  :: Text
  -> [Either Directive Text]
  -> [Either Directive Text]
  -> Parser Directive
restOfIfElse cond a b = choice [ try $ compilerDirective >>= processIfDirectivePart cond a b
                         , do
                            a' <- Left <$> Comment <$> readUpToEndComment 1
                            b' <- Right <$> takeWhileP' (Just "character") (/= '{') <* char '{'
                            let b'' = removeEmpties (b <> [a', b'])
                            restOfIfElse cond a b''
                         ]

removeEmpties = filter (\x -> case x of
                                  Left x -> True
                                  Right "" -> False
                                  otherwise -> True)
removeEmpties' = filter (\x -> case x of
                                  Comment "" -> False
                                  otherwise -> True)

processIfDirectivePart
  :: Text
  -> [Either Directive Text]
  -> [Either Directive Text]
  -> Directive -> Parser Directive
processIfDirectivePart cond a b part = do
  case part of
    UnknownDirective ("endif", _) -> do
      return $ IfDef cond a b
    UnknownDirective ("else", _) -> do
      return $ IfDef cond a b
      els <- takeWhileP' (Just "character") (/= '{') <* char '{'
      restOfIfElse cond a (b <> [Right els])
    otherwise -> do
      -- TODO: Figure out why I can't just use restOfIf as per the above, here.
      rst <- takeWhileP' (Just "character") (/= '{') <* char '{'
      fin <- compilerDirective -- TODO: Or a regular comment.
      let a' = removeEmpties $ a <> [Left otherwise] <> [Right rst]
      processIfDirectivePart cond a' [] fin

restOfBlockComment' :: Text -> Text -> Parser Directive
restOfBlockComment' "{" "}" = Comment <$> readUpToEndComment 1
restOfBlockComment' a b = Comment <$> pack <$> (manyTill anyChar (string b))

-- Removes all spaces after a lexime.
lexeme :: Parser a -> Parser (Lexeme a)
lexeme a = do
  b <- L.lexeme sc a
  c <- optional comment
  return $ Lexeme (fromMaybe NoDirective c)
                  b

symbol' :: Text -> Parser (Lexeme Text)
symbol' a = do
  b <- L.symbol sc a
  c' <- optional comment
  return $ Lexeme (fromMaybe NoDirective c')
                  b

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
semi = void $ semi'

semi' :: Parser Directive
semi' = takeComment <$> symbol' ";"
  where
    takeComment (Lexeme c _) = c

reserved :: [Text]
reserved =
  words
    "and array asm begin break case const constructor continue destructor div do downto else end false file for function goto if implementation in inline interface label mod nil not object of on operator or packed procedure program record repeat set shl shr string then to true type unit until uses var while with xor as class dispose except exit exports finalization finally inherited initialization is library new on out property raise threadvar try absolute abstract alias assembler cdecl cppdecl default export external forward generic index local nostackframe oldfpccall override pascal private protected public published read register reintroduce safecall softfloat specialize stdcall virtual write far near"

rword :: Text -> Parser (Lexeme ())
rword w = lexeme (string' w *> notFollowedBy alphaNumChar)

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

anyIdentifier :: Parser (Lexeme Text)
anyIdentifier = lexeme $ takeWhileP (Just "character") (\c -> isAlphaNum c || c == '_')
