{-# LANGUAGE RankNTypes #-}
module Handler.Utils where
import Prelude
import Database.Esqueleto

import System.Random
import Data.Text (Text)
import qualified Data.Text as T
import Handler.DB.Internal
import Data.Time
import Handler.DB.Enums
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import qualified  Database.Persist as P
import qualified Yesod  as Y
import qualified Data.List as L
import Data.Text.Encoding (decodeASCII)
import Network.Wai (requestHeaders)
import Network.HTTP.Types.Header



isContractValid :: User -> Day -> Bool
isContractValid user today = liftM2 (>) (Just today) (userContractEndDate user) /= Just True && liftM2 (<) (Just today) (userContractStartDate user) /= Just True
getIp = do
    wr <- Y.waiRequest
    return $ (L.find ((=="X-Real-IP") .  fst) $ requestHeaders wr)
        >>= (Just . decodeASCII. snd)

nonEmpty "" = return False
nonEmpty _ = return True

rndString :: Int -> IO Text
rndString len = replicateM len rndChar >>= return . T.pack
    where
        chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A' .. 'Z']
        rndChar = do
            idx <- randomRIO (0, length chars - 1)
            return $ chars !! idx


prepareNewUser :: forall m. (MonadIO m) => UserId -> UserId -> ReaderT SqlBackend m ()
prepareNewUser authId uId = do
    runMaybeT $ do
        auth <- MaybeT $ P.get authId 
        u <- MaybeT $ P.get uId 

        let ugId = userDefaultUserGroupId u
        ug <- MaybeT $ P.get ugId

        authUg <- MaybeT $ P.get $ userDefaultUserGroupId auth

        lift $ do
            insert $ newUserGroupItem ugId uId UserGroupModeReadWrite
            when (userGroupEmail ug == "") $
                P.update ugId [ UserGroupEmail P.=. (userGroupEmail authUg) ] 
    return ()

hasReadPermMaybe :: (UserGroupContentContentIdField e, PersistField (Key e)) => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Maybe (Key e))) -> SqlExpr (Database.Esqueleto.Value Bool)
hasReadPermMaybe aId fld = exists $ from $ \ugc -> do
    where_ $ fld ==. ugc ^. (userGroupContentContentIdField fld)
    where_ $ exists $ from $ \(ug`InnerJoin` ugi) -> do 
        on $ ug ^. UserGroupId ==. ugi ^. UserGroupItemUserGroupId
        where_ $ ugi ^. UserGroupItemUserId ==. aId
        where_ $ not_ $ isNothing $ ugc ^. (userGroupContentContentIdField fld)
        where_ $ isNothing $ ugi ^. UserGroupItemDeletedVersionId
        where_ $ ug ^. UserGroupId ==. ugc ^. UserGroupContentUserGroupId
        where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId

hasReadPerm :: (UserGroupContentContentIdField e, PersistField (Key e))
        => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Key e)) -> SqlExpr (Database.Esqueleto.Value Bool)
hasReadPerm aId fld = hasReadPermMaybe aId (just fld)

hasWritePerm :: (UserGroupContentContentIdField e, PersistField (Key e))
    => SqlExpr (Database.Esqueleto.Value (Key User)) -> SqlExpr (Database.Esqueleto.Value (Key e)) -> SqlExpr (Database.Esqueleto.Value Bool)
hasWritePerm aId fld' = let fld = just fld' in exists $ from $ \ugc -> do
    where_ $ fld ==. ugc ^. (userGroupContentContentIdField fld)
    where_ $ exists $ from $ \(ug`InnerJoin` ugi) -> do 
        on $ ug ^. UserGroupId ==. ugi ^. UserGroupItemUserGroupId
        where_ $ ugi ^. UserGroupItemUserId ==. aId
        where_ $ ugi ^. UserGroupItemMode ==. val UserGroupModeReadWrite
        where_ $ not_ $ isNothing $ ugc ^. (userGroupContentContentIdField fld)
        where_ $ isNothing $ ugi ^. UserGroupItemDeletedVersionId
        where_ $ ug ^. UserGroupId ==. ugc ^. UserGroupContentUserGroupId
        where_ $ isNothing $ ugc ^. UserGroupContentDeletedVersionId
