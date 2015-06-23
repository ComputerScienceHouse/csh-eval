{-|
Module      : CSH.Eval.Frontend
Description : The top level routing of calls to the web viewer
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

module CSH.Eval.Frontend 
( evalFrontend
) where

import qualified Data.Text as T

import CSH.Eval.Routes (evalAPI)
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Blaze  (preEscapedText)
import Yesod
import Yesod.Static

-- Declaration of location of static files
staticFiles "frontend/static"

-- | datatype for Yesod
data EvalFrontend = EvalFrontend
                  { getStatic :: Static
                  }

-- Route definition for the Evaluations Database Website
-- This is where you add new HTML pages
-- The EvalAPI is defined as a subsite of this website, under the /api route.
-- Static files are placed under the /static route, also as a subsite,
-- facilitated by the yesod-static package.
mkYesod "EvalFrontend" [parseRoutes|
/                           HomeR                    GET
/evals/membership/overview  EvalsMembershipOverviewR GET
/api                        EvalSubsiteR WaiSubsite  getEvalAPI
/static                     StaticR Static           getStatic
/projects                   ProjectsR                GET
|]

-- | The basic layout for every CSH Eval page
evalLayout :: Widget -> Handler Html
evalLayout widget = do
    pc <- widgetToPageContent $ do
        widget
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js"
        addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"
        addStylesheet $ StaticR csh_bootstrap_min_css
        toWidget $(luciusFile "frontend/templates/csh-eval.lucius")
    withUrlRenderer $(hamletFile "frontend/templates/base.hamlet")

-- | The Yesod instance for the EvalFrontend
instance Yesod EvalFrontend where
    defaultLayout = evalLayout

-- | Defines the WAI Application for the eval Yesod app
evalFrontend :: IO Application
evalFrontend = do
    s <- static "frontend/static"
    toWaiApp $ EvalFrontend s

-- | A Yesod subsite for the evalAPI defined using servant in "CSH.Eval.Routes"
getEvalAPI :: EvalFrontend -> WaiSubsite
getEvalAPI _ = WaiSubsite evalAPI

-- * Widgets

-- | A helper function to convert integers to text, to make templates that use
-- Integer arguments more readable.
toText :: Integer -> T.Text
toText = T.pack . show

-- | A basic widget for a panel
widgetPanel :: Integer -- horizontal size of the panel
            -> Widget  -- widget for the title of the panel
            -> Widget  -- widget for the body of the panel
            -> Widget
widgetPanel mdsize title body = $(whamletFile "frontend/templates/widgets/panel.hamlet")

-- | A basic widget for a panel that takes text as arguments instead of widgets
widgetPanelText :: Integer -- horizontal size of the panel
                -> T.Text  -- html for the title of the panel
                -> T.Text  -- html for the body of the panel
                -> Widget
widgetPanelText mdsize title body = $(whamletFile "frontend/templates/widgets/panelText.hamlet")

-- * Pages

-- | An example Html handler for the frontend
getHomeR :: Handler Html
getHomeR = defaultLayout $(whamletFile "frontend/templates/index.hamlet")

-- | The page for the Overiview of Membership Evaluations
getEvalsMembershipOverviewR :: Handler Html
getEvalsMembershipOverviewR = defaultLayout $(whamletFile "frontend/templates/evals/membership/overview.hamlet")

-- | The page for a overview of CSH projects.
getProjectsR :: Handler Html
getProjectsR = defaultLayout $(whamletFile "frontend/templates/projects/index.hamlet")
    where projects :: [(String, String, String, String)]
          projects = [("Harlan Haskins", "CSH Eval Stubs", "Stubbed out the projects page.", "In Progress")
                     ,("DuWayne Theroc-Johnson", "Bloodline", "A 1-900 hotline for blood deliveries", "Completed")
                     ] 
