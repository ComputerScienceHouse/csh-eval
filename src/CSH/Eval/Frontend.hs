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
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend 
( evalFrontend
 ,widgetPanel
 ,widgetPanelOffset
 ,widgetPanelText
) where

import qualified Data.Text as T

import CSH.Eval.Routes (evalAPI)
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Blaze  (preEscapedText)
import Yesod
import Yesod.Static
import Yesod.Markdown
import Data.List (unfoldr, find)

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
!/projects/create           CreateProjectR           GET
/projects/#Int              ProjectR                 GET
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
        addStylesheet $ StaticR syntax_css
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
widgetPanel mdsize title body = widgetPanelOffset mdsize 0 title body

widgetPanelOffset :: Integer -> Integer -> Widget -> Widget -> Widget
widgetPanelOffset mdsize mdoffset title body = $(whamletFile "frontend/templates/widgets/panel.hamlet")

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

projects :: [(T.Text, T.Text, T.Text, T.Text, Int)]
projects = (take 100 . cycle)
    [("Harlan Haskins", "Punctual.swift", "## Punctual.swift\n\nPunctual.swift is a set of extensions to `NSDate`, `Int`, `NSTimeInterval`, `NSCalendar`, and `NSDateComponents`.\n```rust\n1.day.until(NSDate())\n```", "In Progress", 4)
    ,("DuWayne Theroc-Johnson", "Bloodline", "# Bloodline\nA 1-900 hotline for blood deliveries. Uses the `MEAN` stack -- `MongoDB`, `Express.js`, `Angularjs` and `Node.js`. That means this project is **truly** web scale.\n```javascript\nfunction thing() {\n  console.log('hello');\n}\n```", "Completed", 5)
    ,("Matt Gambogi", "tyle", "TYped Language Evaluator", "In Progress", 6)
    ] 

projectsCSS = toWidget $(luciusFile "frontend/templates/projects/projects.lucius")

-- | The page for a overview of CSH projects.
getProjectsR :: Handler Html
getProjectsR = defaultLayout $ do
    projectsCSS
    $(whamletFile "frontend/templates/projects/index.hamlet")
    where panel member name description status = widgetPanel 6 (title name member status) (contentPanel description)

title name member status = [whamlet| 
  <strong> 
      #{name} 
  <span .project-status>
      #{status}
  <br />
  <span .project-author>
      by 
      <strong>
          #{member}
|]

contentPanel :: T.Text -> Widget
contentPanel description = [whamlet|
  <div .project-content>
      ^{markdownToHtml $ Markdown description}
|]

getProjectR id = defaultLayout $ do
    projectsCSS
    $(whamletFile "frontend/templates/projects/project.hamlet")
    where fromID = find (\(_, _, _, _, id') -> id == id') projects
          panel = case fromID of
                    Nothing -> notFound
                    Just (member, name, description, status, _) -> widgetPanelOffset 8 2 (title name member status) (contentPanel description)

committees :: [T.Text]
committees = ["Evaluations", "Financial", "OpComm", "House History", "House Improvements", "Research and Development", "Social", "Chairman"]

projectForm = $(whamletFile "frontend/templates/projects/create.hamlet")

getCreateProjectR = defaultLayout $ do
    projectsCSS
    widgetPanelOffset 8 2 [whamlet|New Project|] projectForm
