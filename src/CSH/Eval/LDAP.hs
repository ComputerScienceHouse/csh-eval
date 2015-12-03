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

module CSH.Eval.LDAP (
    module CSH.LDAP
  , lookup
  ) where
import Prelude hiding (lookup)
import CSH.LDAP
import CSH.Eval.Model
import CSH.Eval.Cacheable.Prim
import CSH.Eval.Cacheable.Fetch
import Ldap.Client.Search
import qualified CSH.Eval.Config as Cfg
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Safe
import System.Log.Logger

lookupAttr :: T.Text                  -- ^ Attribute
           -> AttrValue               -- ^ Uid
           -> IO (Maybe B.ByteString)
lookupAttr attr = head . lookupAttrs

lookupAttrs :: [T.Text]                -- ^ Attributes
            -> AttrValue               -- ^ Uid
            -> IO (Maybe [(T.Text, B.ByteString)]
lookupAttrs attrs usr pass uid = val
                             >>= \v -> pure $ case v of
            (Right (Right r)) -> unSearchEntries r
            _                 -> Nothing
    where val = withConfig $ \l -> do
                    searchEither l (Dn userBaseTxt)    (typesOnly False)
                                   (Attr "uid" := uid) [Attr attr]
