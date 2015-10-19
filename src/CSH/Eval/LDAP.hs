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
) where
import Prelude hiding (lookup)
import CSH.LDAP
import Ldap.Client.Search
import qualified CSH.Eval.Config as Cfg
import qualified Data.ByteString.Char8 as B
import Data.Text
import Data.Either
import Safe

lookup attr uid = do
            cfg  <- Cfg.evalConfig
            usr  <- (fmap (fromJustNote "ldap.user DNE in config.")
                                        (Cfg.lookup cfg "ldap.user"))
            pass <- (fmap (fromJustNote "ldap.password DNE in config.")
                                        (Cfg.lookup cfg "ldap.password"))
            lookupAttr attr usr pass uid

extractValue :: [SearchEntry] -> Maybe B.ByteString
extractValue ((SearchEntry _ ((_,(n:_)):_)):_) = Just n
extractValue _                                 = Nothing

lookupAttr :: Text -> Text -> B.ByteString -> AttrValue -> IO (Maybe B.ByteString)
lookupAttr attr usr pass uid = val >>= \v -> return $ case v of
            (Right (Right r)) -> extractValue r
            _                 -> Nothing
    where val = withCSH $ \l -> do
                    bind l (appDn usr) (Password pass)
                    searchEither l (Dn userBaseTxt)    (typesOnly False)
                                   (Attr "uid" := uid) [Attr attr]
