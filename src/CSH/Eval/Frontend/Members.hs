{-
Module      : CSH.Eval.Frontend.Members
Description : The route handler for the members page
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

DOCUMENT THIS!
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Members (
    getMembersR
  , getMemberR
  ) where

import qualified Data.ByteString.Char8 as B
import CSH.Eval.Cacheable.Fetch
import CSH.Eval.Model
import CSH.Eval.Frontend.Data
import qualified CSH.Eval.LDAP as LD
import qualified Data.Text as T
import System.Log.Logger
import Yesod
import Network.HTTP.Types

-- | The handler for the members listing page
getMembersR :: Handler Html
getMembersR = defaultLayout $(whamletFile "frontend/templates/members/index.hamlet")

dummyMembers :: [(String, String, Int, Bool, Bool)]
dummyMembers = take 100 . cycle $ 
               [("harlan", "Harlan Haskins", 4, True, True)
               ,("dag10", "Drew Gottlieb", 4, True, False)
               ,("tmobile", "Travis Whitaker", 8, True, False)
               ]

charFor :: Bool -> T.Text
charFor True = "✅"
charFor False = "❌"

-- | The handler for a single member's page
getMemberR :: String -> Handler Html
getMemberR username = do
           y <- getYesod
           let cache = getCache y
           let logger = getFrontendLogger y
           eitherUsr <- execCacheable cache (getMemberUsername (T.pack username))
           let attendance = [("Evals", "Committee", "10/13/2015"), ("Financial", "Committee", "10/13/2015")]
           case eitherUsr of
            (Left _) -> sendResponseStatus internalServerError500 ("Could not find " ++ username)
            (Right usr) -> defaultLayout $(whamletFile "frontend/templates/index.hamlet")

widgetEval :: Evaluation -> Widget
widgetEval eval = do
    y <- getYesod
    $(whamletFile "frontend/templates/member/widgets/evaluation.hamlet")
