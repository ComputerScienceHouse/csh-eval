{-|
Module      : CSH.Eval.LDAP
Description : LDAP convenience function and examples
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.LDAP provides functions for specific interactions with the CSH
LDAP instance involving the evaluations process.
-}

module CSH.Eval.LDAP
( module CSH.LDAP
, lookup
, profilePhoto
, cn
) where
import Prelude hiding (lookup)
import CSH.LDAP
import qualified CSH.Eval.Config as Cfg
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import Data.Text
import Data.Either
import Safe

lookup f uid = do
            cfg <- Cfg.evalConfig
            usr <- (fmap (fromJustNote "ldap.user DNE in config.")
                                       (Cfg.lookup cfg "ldap.user"))
            pass <- (fmap (fromJustNote "ldap.password DNE in config.")
                                        (Cfg.lookup cfg "ldap.password"))
            f usr pass uid

cn = lookupAttr "cn"

profilePhoto usr pass uid = lookupAttr "jpegPhoto" usr pass uid

lookupAttr :: Text -> Text -> B.ByteString -> AttrValue -> IO B.ByteString
lookupAttr attr usr pass uid = either (B.pack . show) id <$> val
   where val = withCSH $ \l -> do
            bind l (appDn usr) (Password pass)
            ((SearchEntry _ ((_,(n:_)):_)):_) <- search l (Dn userBaseTxt)
                                                          (typesOnly False)
                                                          (Attr "uid" := uid)
                                                          [Attr attr]
            return n
