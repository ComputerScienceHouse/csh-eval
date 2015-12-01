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

-- | The handler for the members listing page
getMembersR :: Handler Html
getMembersR = do
           defaultLayout $(whamletFile "frontend/templates/members/index.hamlet")

dummyMembers :: [(String, String, Int, Bool, Bool)]
dummyMembers = take 100 . cycle $ 
               [("harlan", "Harlan Haskins", 4, True, True)
               ,("dag10", "Drew Gottlieb", 4, True, False)
               ,("tmobile", "Travis Whitaker", 8, True, False)
               ]

charFor :: Bool -> String
charFor True = "✅"
charFor False = "❌"

-- | The handler for a single member's page
getMemberR :: String -> Handler Html
getMemberR user = do
           y <- getYesod
           let cache = getCache y
           let logger = getFrontendLogger y
           liftIO (execCacheable cache (getMemberUsername (T.pack user)) >>= logL logger EMERGENCY . show)
           let usr = B.pack user
           nameEither <- liftIO $ LD.lookup "cn" usr
           let name = B.unpack $ case nameEither of
                       (Just n) -> n
                       Nothing -> usr
           let attendance = [("Evals", "Committee", "10/13/2015"), ("Financial", "Committee", "10/13/2015")]
           let access = Member
           defaultLayout $(whamletFile "frontend/templates/index.hamlet")

widgetEval :: Evaluation -> Widget
widgetEval eval = do
    y <- getYesod
    $(whamletFile "frontend/templates/member/widgets/evaluation.hamlet")
