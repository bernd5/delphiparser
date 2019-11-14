{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE InstanceSigs          #-}

module Web where
import Prelude hiding (head)
import           Yesod
import Data.Text hiding (head)
import Data.List.Safe (head)
import Data.Maybe (mapMaybe)
import Data.ByteString.Lazy (ByteString)
import DelphiAst
import HtmlPretty (showHtml)
import Text.Hamlet (hamletFile)
import Text.Lucius (CssUrl, luciusFile, luciusFileReload, renderCss, Css)
import Imports
import Yesod.Static
import Yesod.Default.Util          (addStaticContentExternal)
import Text.Jasmine                (minifym)

-- Fantastic reference example:
-- https://github.com/commercialhaskell/stack-templates/blob/master/yesod-simple.hsfiles

data DocServer = DocServer { delphiSourcePaths :: [Text]
                           , delphiUnits :: [Unit]
                           , getStatic :: Static
                           }

staticFiles "static/"

mkYesod "DocServer" [parseRoutes|
/ HomeR GET
/units UnitListR GET
/units/#Text UnitInformationR GET

/static StaticR Static getStatic
|]


data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route DocServer
    , menuItemAccessCallback :: Bool
    }
data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

instance YesodBreadcrumbs DocServer where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb UnitListR = return ("Units", Just HomeR)
  breadcrumb (UnitInformationR unitName) = return ("Unit " <> unitName, Just UnitListR)

instance Yesod DocServer where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          addStylesheet $ StaticR css_bootstrap_css
          toWidget $(luciusFileReload "home.lucius")
          $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> ByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route DocServer, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = "static"
        addStaticContentExternal
          minifym
          genFileName
          staticDir
          (StaticR . flip StaticRoute [])
          ext
          mime
          content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

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
      <h2><a href="/units/#{unit}">#{unit}</a>
    <hr>
    Yesod Version #{yesodVersion}|]

  where
    names (Unit _ (Lexeme _ name) _ _ _ _) = name
    names (UnitFragment _ _) = "<fragment>"
    names (Program (Lexeme _ name) _ _ _) = name

getUnitInformationR :: Text -> Handler Html
getUnitInformationR unitName = do
  units <- fmap delphiUnits getYesod
  let unit = head $ mapMaybe byUnitName units
  case unit of
    Just x -> defaultLayout $ do
      toWidget $ $(luciusFileReload "home.lucius")
      toWidget [whamlet|<div id="content">#{showHtml x}|]
    Nothing -> defaultLayout [whamlet|<div id="content">Missing|]
  where
    byUnitName x@(Unit _ (Lexeme _ n) _ _ _ _) = if n == unitName
                                                 then Just x
                                                 else Nothing
    byUnitName x@(Program (Lexeme _ n) _ _ _) = if n == unitName
                                                 then Just x
                                                 else Nothing
    byUnitName x = Nothing


serveWeb :: Int -> [FilePath] -> [Unit] -> IO ()
serveWeb port files units = static "static/" >>= \a -> warp
  port
  (DocServer { delphiSourcePaths = fmap pack files
             , delphiUnits = units
             , getStatic = a })

