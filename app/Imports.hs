module Imports
  ( widgetFile
  )
where

import ClassyPrelude.Yesod
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Yesod.Default.Util          (WidgetFileSettings, widgetFileNoReload,
                                    widgetFileReload)

-- From https://github.com/commercialhaskell/stack-templates/blob/master/yesod-simple.hsfiles
-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if True
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings
