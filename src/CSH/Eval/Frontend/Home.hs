{-|
Module      : CSH.Eval.Frontend.Home
Description : The route handler for the home page
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

DOCUMENT THIS.
-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ViewPatterns      #-}

module CSH.Eval.Frontend.Home (
    getHomeR
  ) where

import qualified Data.ByteString.Char8 as B
import CSH.Eval.Frontend.Data
import CSH.Eval.Frontend.Members
import Network.Wai
import Data.Maybe
import qualified Data.List as L (lookup)
import Yesod

-- | The handler for the Evaluations Database index page.
getHomeR :: Handler Html
getHomeR = do
    req <- waiRequest
    let usr = fromMaybe "" (L.lookup "X-WEBAUTH-USER" (requestHeaders req))
    getMemberR $ B.unpack usr
