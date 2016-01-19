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
module Handler.DB.RouteUsergroups where
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

getUsergroupsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getUsergroupsR  = lift $ runDB $ do
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
    (filterParam_musicPieceIdList) <- lookupGetParam "musicPieceIdList"
    (filterParam_hideDeleted :: Maybe Text) <- lookupGetParam "hideDeleted"
    let baseQuery limitOffsetOrder = from $ \(ug ) -> do
        let ugId' = ug ^. UserGroupId
        where_ (hasReadPerm (val authId) (ug ^. UserGroupId))

        _ <- when limitOffsetOrder $ do
            offset 0
            limit 10000
            case defaultSortJson of 
                Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                        "email" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ug  ^.  UserGroupEmail) ] 
                            "DESC" -> orderBy [ desc (ug  ^.  UserGroupEmail) ] 
                            _      -> return ()
                        "organization" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ug  ^.  UserGroupOrganization) ] 
                            "DESC" -> orderBy [ desc (ug  ^.  UserGroupOrganization) ] 
                            _      -> return ()
                        "current" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ug  ^.  UserGroupCurrent) ] 
                            "DESC" -> orderBy [ desc (ug  ^.  UserGroupCurrent) ] 
                            _      -> return ()
                        "name" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (ug  ^.  UserGroupName) ] 
                            "DESC" -> orderBy [ desc (ug  ^.  UserGroupName) ] 
                            _      -> return ()
            
                        _ -> return ()
                    ) xs
                Nothing -> orderBy [ asc (ug ^. UserGroupName) ]

            case defaultOffset of
                Just o -> offset o
                Nothing -> return ()
            case defaultLimit of
                Just l -> limit (min 10000 l)
                Nothing -> return ()
                 
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case FS.f_field fjm of
                "id" -> case (FS.f_value fjm >>= PP.fromPathPiece)  of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupId) (val v')
                    _        -> return ()
                "email" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupEmail) ((val v'))
                    _        -> return ()
                "organization" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupOrganization) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupOrganization) nothing
                           
                "current" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupCurrent) ((val v'))
                    _        -> return ()
                "name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (ug  ^.  UserGroupName) ((val v'))
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (ug ^. UserGroupName) `ilike` ((((val "%")) ++. ((val (localParam :: Text)))) ++. ((val "%")))
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_musicPieceIdList defaultFilterJson "musicPieceIdList" of
            Just localParam -> do 
                
                where_ $ (ug ^. UserGroupId) `in_` (subList_select $ from $ \(ugi) -> do {  ; where_ (((ugi ^. UserGroupItemUserId) `in_` (valList localParam)) &&. ((ugi ^. UserGroupItemDeletedVersionId) `is` (nothing))) ; return (ugi ^. UserGroupItemUserGroupId) ; })
            Nothing -> return ()
        if FS.hasDefaultFilter filterParam_hideDeleted defaultFilterJson "hideDeleted" 
            then do 
                 
                where_ $ (ug ^. UserGroupDeletedVersionId) `is` (nothing)
            else return ()
        return (ug ^. UserGroupId, ug ^. UserGroupEmail, ug ^. UserGroupOrganization, ug ^. UserGroupCurrent, ug ^. UserGroupName)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    (return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5)) -> A.object [
                    "id" .= toJSON f1,
                    "email" .= toJSON f2,
                    "organization" .= toJSON f3,
                    "current" .= toJSON f4,
                    "name" .= toJSON f5                                    
                    ]
                _ -> A.object []
            ) results)
       ])
postUsergroupsR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postUsergroupsR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_email <- case HML.lookup "email" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute email in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute email in the JSON object in request body" :: Text)
            ]
    attr_name <- case HML.lookup "name" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute name in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute name in the JSON object in request body" :: Text)
            ]
    __currentTime <- liftIO $ getCurrentTime
    (Entity _ __auth) <- lift $ requireAuth
    runDB_result <- do
        e1 <- do
    
            return $ UserGroup {
                            userGroupEmail = attr_email
                    ,
                            userGroupOrganization = Nothing
                    ,
                            userGroupCurrent = Active
                    ,
                            userGroupName = attr_name
                    ,
                            userGroupActiveId = Nothing
                    ,
                            userGroupActiveStartTime = (Just __currentTime)
                    ,
                            userGroupActiveEndTime = Nothing
                    ,
                            userGroupDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e1
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_ugId <- P.insert (e1 :: UserGroup)
        e2 <- do
    
            return $ UserGroupContent {
                            userGroupContentUserGroupId = (userDefaultUserGroupId __auth)
                    ,
                            userGroupContentFileContentId = Nothing
                    ,
                            userGroupContentUserGroupContentId = (Just result_ugId)
                    ,
                            userGroupContentUserContentId = Nothing
                    ,
                            userGroupContentCompanyContentId = Nothing
                    ,
                            userGroupContentDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e2 :: UserGroupContent)
        return $ A.object [ "id" .= (toJSON result_ugId) ]
    return $ runDB_result
