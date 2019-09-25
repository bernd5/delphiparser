module Args where

import           Options.Applicative hiding (maybeReader)
import           Options.Applicative.Types (readerAsk)
import           Text.Read
import           Data.Semigroup                 ( (<>) )
import           Data.Text

data Args = Args
          { file :: Maybe Text
          , dir :: Maybe Text
          , docWeb :: Maybe Int
          , showTypes :: Bool
          }

args :: Parser Args
args =
  Args
    <$> maybeOption
          (long "file" <> short 'f' <> value Nothing <> metavar "FILE" <> help
            "Pascal file to parse"
          )
    <*> maybeOption
          (long "dir" <> short 'd' <> value Nothing <> metavar "DIR" <> help
            "Directory to scan to locate pascal files for parsing"
          )
    <*> maybeReader
          (  long "docweb"
          <> short 'p'
          <> value Nothing
          <> metavar "DOCWEB"
          <> help "Start Pascal docweb"
          )
    <*> switch
          (long "show-types" <> short 't' <> help
            "Show a list of the discovered types"
          )

toMaybeText :: String -> Maybe Text
toMaybeText s = Just $ pack s

toMaybe :: Read a => String -> Maybe a
toMaybe a = Just $ read a

maybeOption :: Mod OptionFields (Maybe Text) -> Parser (Maybe Text)
maybeOption = option (fmap toMaybeText readerAsk)

maybeReader :: Read a => Mod OptionFields (Maybe a) -> Parser (Maybe a)
maybeReader = option (fmap toMaybe readerAsk)

getArgs = execParser opts
 where
  opts = info
    (args <**> helper)
    (fullDesc <> progDesc "Pascal Parser and AST Generator" <> header
      "past: Pascal Parser and AST Generator"
    )
