{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Data.Text as T

data DocServer = DocServer { delphiSourcePaths :: [Text]}

mkYesod "DocServer" [parseRoutes|
/ HomeR GET
|]

instance Yesod DocServer

getHomeR :: Handler Html
getHomeR = do
    sources <- foo
    defaultLayout [whamlet|
<h1>Object Pascal Documentation Server
<h2>#{sources}
<ul>
    <li>One
    <li>Two
#{yesodVersion}|]
    where
        foo :: Handler Text
        foo = do
            sources <- fmap delphiSourcePaths getYesod
            return $ intercalate " " sources

main :: IO ()
main = warp 3000 (DocServer { delphiSourcePaths = ["hey"]})