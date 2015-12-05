module LdapImport
where
import CSH.LDAP
import CSH.Eval.Model
import CSH.Eval.Cacheable.Prim
import CSH.Eval.Cacheable.Make
import Data.UUID
import Data.Maybe
import Data.ByteString.Char8 as B
import Data.Text as T
import Ldap.Client.Search
import System.Log.Logger

ldapImport = withConfig $ \l -> do
    lgr <- getRootLogger
    cache <- initCacheFromConfig lgr
    ldapres <- search l (Dn userBaseTxt)
                        (typesOnly False)
                        (Present (Attr "uid"))
                        [ Attr "uid"
                        , Attr "cn"
                        , Attr "entryUUID"
                        , Attr "housingPoints"
                        ]
    let attrlst = fmap (\(SearchEntry _ x)-> x) ldapres
    let usrs = fmap (fmap (\((Attr x), (y:_))-> (T.unpack x, B.unpack y))) attrlst
    mapM_ (mkUsrFromLDAP cache logger) usrs

mkUsrFromLDAP cache logger usr = do
    let uuid = fromJust (fromString (jlookup "entryUUID" usr))
    let uid  = T.pack (jlookup "uid" usr)
    let cn   =  maybe "" T.pack (lookup "cn" usr)
    let pts  = (read (maybe "0" id (lookup "housingPoints" usr)) :: Int)
    execCacheable cache (mkExtantMember uuid uid cn pts False)
  where
    jlookup x xs = fromJust (lookup x xs)
