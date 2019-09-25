{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns #-}
module Web where
import           Yesod
import Data.Text
import DelphiAst

data DocServer = DocServer { delphiSourcePaths :: [Text]
                           , delphiUnits :: [Unit]
                           }

mkYesod "DocServer" [parseRoutes|
/ HomeR GET
/units UnitListR GET
/units/#Text UnitInformationR GET
|]

instance Yesod DocServer

getHomeR :: Handler Html
getHomeR = do
    sources <- fmap delphiSourcePaths getYesod
    defaultLayout [whamlet|
<h1>Object Pascal Documentation Server
$forall source <- sources
  <h2>#{source}
<hr>
Yesod Version #{yesodVersion}|]

getUnitListR :: Handler Html
getUnitListR = do
  units' <- fmap delphiUnits getYesod
  let units = fmap names units'
  defaultLayout [whamlet|
    <h1>Object Pascal Documentation Server
    $forall unit <- units
      <h2>#{unit}
    <hr>
    Yesod Version #{yesodVersion}|]

  where
    names (Unit _ (Lexeme _ name) _ _ _ _) = name
    names (UnitFragment _ _) = "<fragment>"
    names (Program (Lexeme _ name) _) = name

getUnitInformationR :: Text -> Handler Html
getUnitInformationR unitName = do
  units <- fmap delphiUnits getYesod
  let hUnits = show $ Prelude.head units
  defaultLayout [whamlet|
    <h1>#{unitName}
    #{hUnits}
|]


serveWeb :: Int -> [FilePath] -> [Unit] -> IO ()
serveWeb port files units = warp
  port
  (DocServer {delphiSourcePaths = fmap pack files, delphiUnits = units})
