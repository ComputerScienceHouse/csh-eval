{-|
Module      : CSH.Eval.Frontend.Home
Description : The route handler for the home page
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Projects
( getHomeR
) where

import qualified Data.Text as T

import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Widgets
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod

-- | An example Html handler for the frontend
getHomeR :: Handler Html
getHomeR = defaultLayout $(whamletFile "frontend/templates/index.hamlet")
