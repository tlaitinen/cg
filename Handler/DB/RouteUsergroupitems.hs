{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Handler.DB.RouteUsergroupitems where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import Handler.DB.Internal
import Handler.DB.Validation
import qualified Handler.DB.FilterSort as FS
import qualified Handler.DB.PathPieces as PP
import Prelude
import Database.Esqueleto
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist, AuthEntity)
import Yesod.Core hiding (fileName, fileContentType)
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)
import Control.Monad (when)
import Data.Aeson ((.:), (.:?), (.!=), FromJSON, parseJSON, decode)
import Data.Aeson.TH
import Data.Int
import Data.Word
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import qualified Data.Attoparsec as AP
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import qualified Data.Text.Read
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List as DL
import Control.Monad (mzero, forM_)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as C
import qualified Network.Wai as W
import Data.Conduit.Lazy (lazyConsume)
import Network.HTTP.Types (status200, status400, status403, status404)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Applicative ((<$>), (<*>))  
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashMap.Strict as HMS
import Handler.Utils (nonEmpty)
import Handler.Utils (prepareNewUser,hasWritePerm,hasReadPermMaybe,hasReadPerm)

getUsergroupitemsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getUsergroupitemsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    defaultFilterParam <- lookupGetParam "filter"
    let defaultFilterJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultFilterParam) :: Maybe [FS.Filter]
    defaultSortParam <- lookupGetParam "sort"
    let defaultSortJson = (maybe Nothing (decode . LBS.fromChunks . (:[]) . encodeUtf8) defaultSortParam) :: Maybe [FS.Sort]
    defaultOffsetParam <- lookupGetParam "start"
    defaultLimitParam <- lookupGetParam "limit"
    let defaultOffset = (maybe Nothing PP.fromPathPiece defaultOffsetParam) :: Maybe Int64
    let defaultLimit = (maybe Nothing PP.fromPathPiece defaultLimitParam) :: Maybe Int64
    (filterParam_query) <- lookupGetParam "query"
    (filterParam_userGroupId) <- lookupGetParam "userGroupId"
    (filterParam_userIdList) <- lookupGetParam "userIdList"
    let baseQuery limitOffsetOrder = from $ \(ugi `InnerJoin` ug`InnerJoin` u) -> do
        on ((ugi ^. UserGroupItemUserId) ==. (u ^. UserId))
        on ((ugi ^. UserGroupItemUserGroupId) ==. (ug ^. UserGroupId))
        let ugiId' = ugi ^. UserGroupItemId
        where_ (((ugi ^. UserGroupItemDeletedVersionId) `is` (nothing)) &&. (hasReadPerm (val authId) (ug ^. UserGroupId)))

        _ <- when limitOffsetOrder $ do
            offset 0
            limit 10000
            case defaultSortJson of 
                Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                        "userName" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserName) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserName) ] 
                            _      -> return ()
                        "userGroupName" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ug  ^.  UserGroupName) ] 
                            "DESC" -> orderBy [ desc (ug  ^.  UserGroupName) ] 
                            _      -> return ()
                        "mode" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ugi  ^.  UserGroupItemMode) ] 
                            "DESC" -> orderBy [ desc (ugi  ^.  UserGroupItemMode) ] 
                            _      -> return ()
            
                        _ -> return ()
                    ) xs
                Nothing -> orderBy [ asc (u ^. UserName) ]

            case defaultOffset of
                Just o -> offset o
                Nothing -> return ()
            case defaultLimit of
                Just l -> limit (min 10000 l)
                Nothing -> return ()
                 
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case FS.f_field fjm of
                "id" -> case (FS.f_value fjm >>= PP.fromPathPiece)  of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ugi  ^.  UserGroupItemId) (val v')
                    _        -> return ()
                "firstName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserFirstName) ((val v'))
                    _        -> return ()
                "lastName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserLastName) ((val v'))
                    _        -> return ()
                "u.organization" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserOrganization) ((val v'))
                    _        -> return ()
                "admin" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserAdmin) ((val v'))
                    _        -> return ()
                "u.email" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserEmail) ((val v'))
                    _        -> return ()
                "contractStartDate" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserContractStartDate) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserContractStartDate) nothing
                           
                "contractEndDate" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserContractEndDate) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserContractEndDate) nothing
                           
                "defaultUserGroupId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserDefaultUserGroupId) ((val v'))
                    _        -> return ()
                "timeZone" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserTimeZone) ((val v'))
                    _        -> return ()
                "u.current" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserCurrent) ((val v'))
                    _        -> return ()
                "config" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserConfig) ((val v'))
                    _        -> return ()
                "strictEmailCheck" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserStrictEmailCheck) ((val v'))
                    _        -> return ()
                "u.name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserName) ((val v'))
                    _        -> return ()
                "createPeriods" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupCreatePeriods) ((val v'))
                    _        -> return ()
                "ug.email" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupEmail) ((val v'))
                    _        -> return ()
                "ug.organization" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupOrganization) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupOrganization) nothing
                           
                "ug.current" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupCurrent) ((val v'))
                    _        -> return ()
                "ug.name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupName) ((val v'))
                    _        -> return ()
                "userGroupId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ugi  ^.  UserGroupItemUserGroupId) ((val v'))
                    _        -> return ()
                "userId" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ugi  ^.  UserGroupItemUserId) ((val v'))
                    _        -> return ()
                "mode" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ugi  ^.  UserGroupItemMode) ((val v'))
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (u ^. UserName) `ilike` ((((val "%")) ++. ((val (localParam :: Text)))) ++. ((val "%")))
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_userGroupId defaultFilterJson "userGroupId" of
            Just localParam -> do 
                
                where_ $ (ugi ^. UserGroupItemUserGroupId) ==. (val localParam)
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_userIdList defaultFilterJson "userIdList" of
            Just localParam -> do 
                
                where_ $ (ugi ^. UserGroupItemUserId) `in_` (valList localParam)
            Nothing -> return ()
        return (ugi ^. UserGroupItemId, u ^. UserId, u ^. UserName, ug ^. UserGroupId, ug ^. UserGroupName, ugi ^. UserGroupItemMode)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    (return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6)) -> A.object [
                    "id" .= toJSON f1,
                    "userId" .= toJSON f2,
                    "userName" .= toJSON f3,
                    "userGroupId" .= toJSON f4,
                    "userGroupName" .= toJSON f5,
                    "mode" .= toJSON f6                                    
                    ]
                _ -> A.object []
            ) results)
       ])
postUsergroupitemsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postUsergroupitemsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_userGroupId <- case HML.lookup "userGroupId" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute userGroupId in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute userGroupId in the JSON object in request body" :: Text)
            ]
    attr_userId <- case HML.lookup "userId" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute userId in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute userId in the JSON object in request body" :: Text)
            ]
    attr_mode <- case HML.lookup "mode" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute mode in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute mode in the JSON object in request body" :: Text)
            ]
    _ <- do
        result <- select $ from $ \(ug ) -> do
            let ugId' = ug ^. UserGroupId
            where_ (((ug ^. UserGroupId) ==. (val attr_userGroupId)) &&. (hasWritePerm (val authId) (ug ^. UserGroupId)))

            limit 1
            return ug
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        delete $ from $ (\ugi -> where_ $ ((ugi ^. UserGroupItemUserGroupId) ==. (val attr_userGroupId)) &&. ((ugi ^. UserGroupItemUserId) ==. (val attr_userId)))
        e3 <- do
    
            return $ UserGroupItem {
                            userGroupItemUserGroupId = attr_userGroupId
                    ,
                            userGroupItemUserId = attr_userId
                    ,
                            userGroupItemMode = attr_mode
                    ,
                            userGroupItemDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e3
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e3 :: UserGroupItem)
        return AT.emptyObject
    return $ runDB_result
