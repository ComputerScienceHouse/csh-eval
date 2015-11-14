{-|
Module      : CSH.Eval.Frontend.Attendance
Description : The route handler for the attendance page
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

module CSH.Eval.Frontend.Attendance (
    getAttendanceR
  ) where

import CSH.Eval.Frontend.Data
-- import qualified Data.List as L (lookup)
import Yesod

-- | The handler for the Evaluations Database index page
getAttendanceR :: Handler Html
getAttendanceR = do
           -- req <- waiRequest
           -- let usr = fromMaybe "" (L.lookup "X-WEBAUTH-USER" (requestHeaders req))
           let committees = ["Evaluations"
                            , "Research and Development"
                            , "House Improvements"
                            , "Social"
                            , "Financial"
                            , "House History"
                            , "Operational Communications"] :: [String]
           defaultLayout $(whamletFile "frontend/templates/attendance/index.hamlet")
