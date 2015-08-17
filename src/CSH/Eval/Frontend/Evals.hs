{-|
Module      : CSH.Eval.Frontend.Evals
Description : Evaluations related route handlers
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

Route handlers for the various Evaluations pages, such as Freshmen evals and membership evals
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Evals
( getEvalsMembershipOverviewR
) where

import qualified Data.Text as T

import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Widgets
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod

-- | The page for the Overiview of Membership Evaluations
getEvalsMembershipOverviewR :: Handler Html
getEvalsMembershipOverviewR = defaultLayout $(whamletFile "frontend/templates/evals/membership/overview.hamlet")
