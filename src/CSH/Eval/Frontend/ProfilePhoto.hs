{-|
Module      : CSH.Eval.Frontend.Home
Description : The route handler for the home page
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

module CSH.Eval.Frontend.ProfilePhoto (
    getProfilePhotoR
  ) where

import qualified CSH.Eval.LDAP as LD
import qualified Data.ByteString.Char8 as B
import Crypto.Hash
import CSH.Eval.Frontend.Data
import Yesod

md5 :: B.ByteString -> Digest MD5
md5 = hash

hashedEmail :: String -> String
hashedEmail = B.unpack . digestToHexByteString . md5 . B.pack  . (flip (++)) "@csh.rit.edu"

gravatarURL :: String -> String
gravatarURL usr = "https://gravatar.com/avatar/" ++ (hashedEmail usr) ++ "?d=mm&size=300"

-- | The handler for the Evaluations Database index page
getProfilePhotoR :: String -> Handler Html
getProfilePhotoR usr = do
           photoData <- liftIO $ LD.lookup "jpegPhoto" (B.pack usr)
           case photoData of
               (Just photo) -> sendResponse (typePng, toContent photo)
               Nothing      -> redirect $ gravatarURL usr
