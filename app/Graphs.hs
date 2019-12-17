{-# LANGUAGE OverloadedStrings #-}
module Graphs where

-- TODO: Use twopi units.dot -ounits.png -Tpng
-- to generate the resulting .png

import DelphiAst
import Web (names)
import Data.Text.IO as TIO
import Data.Text as T hiding (map, concatMap)

generateUnitReferencesDots :: FilePath -> [(FilePath, Unit)] -> IO ()
generateUnitReferencesDots output units = do
  TIO.writeFile output $ intercalate "\n" contents

  where
    contents :: [Text]
    contents = [ "digraph G {"
              -- Generate the unit labels...
               , intercalate "\n"
                    $ map (\x -> "\"" <> x <> "\";")
                    $ map (names . snd) units
              -- Now, define how they relate to each other.
               , intercalate "\n"
                  $ concatMap (toDependencies . snd) units
               , "}"
               ]

toDependencies :: Unit -> [Text]
toDependencies (Unit _ (Lexeme _ name) (Interface (Uses deps _) _) (Implementation (Uses implDeps _) _) _ _) = (
    map
      (\x -> T.concat ["\""
                      , name
                      , "\""
                      , " -> "
                      , "\""
                      , x
                      , "\";"
                      ]) deps'
    ) <> (map
      (\x -> T.concat ["\""
                      , name
                      , "\""
                      , " -> "
                      , "\""
                      , x
                      , "\" [style=dotted];"
                      ]) implDeps'
   )
  where
    deps' = map (\(Lexeme _ x) -> x) deps
    implDeps' = map (\(Lexeme _ x) -> x) implDeps

toDependencies _ = []

