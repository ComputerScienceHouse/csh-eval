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

module CSH.Eval.Frontend.ProfilePhoto
( getProfilePhotoR
) where

import qualified CSH.Eval.LDAP as LD
import qualified Data.ByteString.Char8 as B
import CSH.Eval.Frontend.Data
import CSH.Eval.Config
import Network.Wai
import Yesod

-- | The handler for the Evaluations Database index page
getProfilePhotoR :: String -> Handler Html
getProfilePhotoR usr = do
           photoData <- liftIO $ LD.lookup LD.profilePhoto (B.pack usr)
           sendResponse (typePng, toContent photoData)
