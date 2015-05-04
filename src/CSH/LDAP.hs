{-# LANGUAGE OverloadedStrings #-}
module CSH.LDAP
( userBaseTxt
, appBaseTxt
, groupBaseTxt
, committeeBaseTxt
, appDn
, userDn
, withCSH
) where

import Ldap.Client
import Data.Text as T

-- | Generates a base given an Org Unit (ou) name.
cshBaseTxt :: T.Text -- ^ ou name
           -> T.Text
cshBaseTxt str = T.concat ["ou=", str, ",dc=csh,dc=rit,dc=edu"]

-- | Distinct name for users
userBaseTxt :: T.Text
userBaseTxt = cshBaseTxt "users"

-- | Distinct name for apps
appBaseTxt :: T.Text
appBaseTxt = cshBaseTxt "apps"

-- | Distinct name for groups
groupBaseTxt :: T.Text
groupBaseTxt = cshBaseTxt "groups"

-- | Distinct name for committees
committeeBaseTxt :: T.Text
committeeBaseTxt = cshBaseTxt "committees"

-- | Creates a Dn based on the name of the application
appDn :: Text -- ^ Common name of app
      -> Dn
appDn app = Dn $ T.concat ["cn=", app, ",", appBaseTxt]

-- | Creates a Dn based on the uid of the user
userDn :: Text -- ^ uid of user
       -> Dn
userDn user = Dn $ T.concat ["uid=", user, ",", userBaseTxt]

-- | Wraps LDAP transactions.
withCSH :: (Ldap -> IO a) -> IO (Either LdapError a)
withCSH = with (Secure "ldap.csh.rit.edu") 636
