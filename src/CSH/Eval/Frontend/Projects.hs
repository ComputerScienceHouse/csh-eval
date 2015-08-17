{-|
Module      : CSH.Eval.Frontend.Projects
Description : Project related route handlers
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

module CSH.Eval.Frontend.Projects
( getProjectR
, getProjectsR
, getCreateProjectR
) where

import qualified Data.Text as T

import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Widgets
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod
import Yesod.Markdown
import Data.List (unfoldr, find)

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

-- TODO handle parse error correctly
contentPanel :: T.Text -> Widget
contentPanel description = [whamlet|
  <div .project-content>
      ^{either (error . show) id $ markdownToHtml $ Markdown description}
|]

getProjectR id = defaultLayout $ do
    projectsCSS
    panel
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
