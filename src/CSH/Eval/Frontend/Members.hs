{-
Module      : CSH.Eval.Frontend.Members
Description : The route handler for the members page
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

module CSH.Eval.Frontend.Members
( getMembersR
, getMemberR
) where

import qualified Data.ByteString.Char8 as B
import CSH.Eval.Config
import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Projects
import CSH.Eval.Frontend.ProfilePhoto
import CSH.Eval.Frontend.Widgets
import qualified CSH.Eval.LDAP as LD
import Network.Wai
import Data.Maybe
import qualified Data.List as L (lookup)
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Yesod

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
           let usr = B.pack user 
           nameEither <- liftIO $ LD.lookup "cn" usr
           let name = B.unpack $ case nameEither of
                       (Just n) -> n
                       Nothing -> usr
           let attendance = [("Evals", "Committee", "10/13/2015"), ("Financial", "Committee", "10/13/2015")]
           let access = Member
           defaultLayout $(whamletFile "frontend/templates/index.hamlet")

getMembersR :: Handler Html
getMembersR = do
           defaultLayout $(whamletFile "frontend/templates/members/index.hamlet")
