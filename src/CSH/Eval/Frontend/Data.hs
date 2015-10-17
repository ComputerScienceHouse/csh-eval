{-|
Module      : CSH.Eval.Frontend.Data
Description : Yesod data declarations for the EvalFrontend site
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Defines the web application layer of Evals
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Data where

import CSH.Eval.Routes (evalAPI)
import CSH.Eval.Config (ServerCmd (..))
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod
import Yesod.Static

-- Declaration of location of static files
staticFiles "frontend/static"

-- | datatype for Yesod
data EvalFrontend = EvalFrontend
                  { getStatic :: Static
                  , getConfig :: ServerCmd
                  }

data AccessLvl = Freshman
               | Member
               | Eboard
               | Admin
               deriving (Eq, Ord)

-- This makes the datatypes for the evaluations frontend, for use by
-- page hanlders in different files without having recursive importing.
mkYesodData "EvalFrontend" $(parseRoutesFile "config/routes")

-- | A Yesod subsite for the evalAPI defined using servant in "CSH.Eval.Routes"
getEvalAPI :: EvalFrontend -> WaiSubsite
getEvalAPI _ = WaiSubsite evalAPI

-- | The basic layout for every CSH Eval page
evalLayout :: Widget -> Handler Html
evalLayout widget = do
    pc <- widgetToPageContent $ do
        widget
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
        addStylesheet $ StaticR csh_bootstrap_min_css
        addStylesheet $ StaticR syntax_css
        addStylesheet EvalCssR
    withUrlRenderer $(hamletFile "frontend/templates/base.hamlet")

-- | The Yesod instance for the EvalFrontend
instance Yesod EvalFrontend where
    defaultLayout = evalLayout
    makeSessionBackend s = if tls then ssl "key.aes" else nossl
        where tls   = withTLS (getConfig s)
              ssl   = sslOnlySessions . fmap Just . defaultClientSessionBackend 120
              nossl = return Nothing
    yesodMiddleware = ssl . defaultYesodMiddleware
        where ssl s = withTLS . getConfig <$> getYesod >>=
                      (\x -> if x
                      then sslOnlyMiddleware 120 s
                      else s)
