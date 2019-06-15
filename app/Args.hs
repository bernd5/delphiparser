module Args where

import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           Text.Read
import           Data.Semigroup                 ( (<>) )
import           Data.Text

data Args = Args
          { file :: Maybe Text
          , dir :: Maybe Text
          }

args :: Parser Args
args =
  Args
    <$> maybeOption
          (long "file" <> value Nothing <> metavar "FILE" <> help "Pascal file to parse")
    <*> maybeOption
          (long "dir" <> value Nothing <> metavar "DIR" <> help
            "Directory to scan to locate pascal files for parsing"
          )

toMaybeText s = Just $ pack s

maybeOption :: Mod OptionFields (Maybe Text) -> Parser (Maybe Text)
maybeOption = option (fmap toMaybeText readerAsk)

getArgs = execParser opts
 where
  opts = info
    (args <**> helper)
    (fullDesc <> progDesc "Pascal Parser and AST Generator" <> header
      "past: Pascal Parser and AST Generator"
    )
