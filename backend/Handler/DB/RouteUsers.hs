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
module Handler.DB.RouteUsers where
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

getUsersR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getUsersR  = lift $ runDB $ do
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
    (filterParam_hideDeleted :: Maybe Text) <- lookupGetParam "hideDeleted"
    let baseQuery limitOffsetOrder = from $ \(u ) -> do
        let uId' = u ^. UserId
        where_ (hasReadPerm (val authId) (u ^. UserId))

        _ <- when limitOffsetOrder $ do
            offset 0
            limit 10000
            case defaultSortJson of 
                Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                        "name" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserName) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserName) ] 
                            _      -> return ()
                        "firstName" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserFirstName) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserFirstName) ] 
                            _      -> return ()
                        "lastName" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserLastName) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserLastName) ] 
                            _      -> return ()
                        "organization" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserOrganization) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserOrganization) ] 
                            _      -> return ()
                        "timeZone" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserTimeZone) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserTimeZone) ] 
                            _      -> return ()
                        "defaultUserGroupId" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserDefaultUserGroupId) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserDefaultUserGroupId) ] 
                            _      -> return ()
                        "email" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserEmail) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserEmail) ] 
                            _      -> return ()
                        "strictEmailCheck" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (u  ^.  UserStrictEmailCheck) ] 
                            "DESC" -> orderBy [ desc (u  ^.  UserStrictEmailCheck) ] 
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
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserId) (val v')
                    _        -> return ()
                "firstName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserFirstName) ((val v'))
                    _        -> return ()
                "lastName" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserLastName) ((val v'))
                    _        -> return ()
                "organization" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserOrganization) ((val v'))
                    _        -> return ()
                "admin" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserAdmin) ((val v'))
                    _        -> return ()
                "email" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
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
                "current" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserCurrent) ((val v'))
                    _        -> return ()
                "config" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserConfig) ((val v'))
                    _        -> return ()
                "strictEmailCheck" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserStrictEmailCheck) ((val v'))
                    _        -> return ()
                "name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (u  ^.  UserName) ((val v'))
                    _        -> return ()

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (u ^. UserName) `ilike` ((((val "%")) ++. ((val (localParam :: Text)))) ++. ((val "%")))
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_userGroupId defaultFilterJson "userGroupId" of
            Just localParam -> from $ \(ugi) -> do
 
                where_ ((u ^. UserId) ==. (ugi ^. UserGroupItemUserId))
                
                where_ $ (ugi ^. UserGroupItemUserGroupId) ==. (val localParam)
            Nothing -> return ()
        if FS.hasDefaultFilter filterParam_hideDeleted defaultFilterJson "hideDeleted" 
            then do 
                 
                where_ $ (u ^. UserDeletedVersionId) `is` (nothing)
            else return ()
        return (u ^. UserId, u ^. UserName, u ^. UserFirstName, u ^. UserLastName, u ^. UserOrganization, u ^. UserTimeZone, u ^. UserDefaultUserGroupId, u ^. UserEmail, u ^. UserStrictEmailCheck)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    (return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7), (Database.Esqueleto.Value f8), (Database.Esqueleto.Value f9)) -> A.object [
                    "id" .= toJSON f1,
                    "name" .= toJSON f2,
                    "firstName" .= toJSON f3,
                    "lastName" .= toJSON f4,
                    "organization" .= toJSON f5,
                    "timeZone" .= toJSON f6,
                    "defaultUserGroupId" .= toJSON f7,
                    "email" .= toJSON f8,
                    "strictEmailCheck" .= toJSON f9                                    
                    ]
                _ -> A.object []
            ) results)
       ])
postUsersR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
postUsersR  = lift $ runDB $ do
    authId <- lift $ requireAuthId
    jsonResult <- parseJsonBody
    jsonBody <- case jsonResult of
         A.Error err -> sendResponseStatus status400 $ A.object [ "message" .= ( "Could not decode JSON object from request body : " ++ err) ]
         A.Success o -> return o
    jsonBodyObj <- case jsonBody of
        A.Object o -> return o
        v -> sendResponseStatus status400 $ A.object [ "message" .= ("Expected JSON object in the request body, got: " ++ show v) ]
    attr_strictEmailCheck <- case HML.lookup "strictEmailCheck" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute strictEmailCheck in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute strictEmailCheck in the JSON object in request body" :: Text)
            ]
    attr_timeZone <- case HML.lookup "timeZone" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute timeZone in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute timeZone in the JSON object in request body" :: Text)
            ]
    attr_defaultUserGroupId <- case HML.lookup "defaultUserGroupId" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute defaultUserGroupId in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute defaultUserGroupId in the JSON object in request body" :: Text)
            ]
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
    attr_organization <- case HML.lookup "organization" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute organization in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute organization in the JSON object in request body" :: Text)
            ]
    attr_lastName <- case HML.lookup "lastName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute lastName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute lastName in the JSON object in request body" :: Text)
            ]
    attr_firstName <- case HML.lookup "firstName" jsonBodyObj of 
        Just v -> case A.fromJSON v of
            A.Success v' -> return v'
            A.Error err -> sendResponseStatus status400 $ A.object [
                    "message" .= ("Could not parse value from attribute firstName in the JSON object in request body" :: Text),
                    "error" .= err
                ]
        Nothing -> sendResponseStatus status400 $ A.object [
                "message" .= ("Expected attribute firstName in the JSON object in request body" :: Text)
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
    _ <- do
        result <- select $ from $ \(u ) -> do
            let uId' = u ^. UserId
            where_ (((u ^. UserAdmin) ==. ((val True))) &&. ((u ^. UserId) ==. (val authId)))

            limit 1
            return u
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        e2 <- do
    
            return $ User {
                            userFirstName = attr_firstName
                    ,
                            userLastName = attr_lastName
                    ,
                            userOrganization = attr_organization
                    ,
                            userAdmin = False
                    ,
                            userEmail = attr_email
                    ,
                            userPassword = ""
                    ,
                            userSalt = ""
                    ,
                            userPasswordResetToken = Nothing
                    ,
                            userPasswordResetValidUntil = Nothing
                    ,
                            userContractStartDate = Nothing
                    ,
                            userContractEndDate = Nothing
                    ,
                            userDefaultUserGroupId = attr_defaultUserGroupId
                    ,
                            userTimeZone = attr_timeZone
                    ,
                            userCurrent = Active
                    ,
                            userConfig = "{}"
                    ,
                            userStrictEmailCheck = attr_strictEmailCheck
                    ,
                            userName = attr_name
                    ,
                            userActiveId = Nothing
                    ,
                            userActiveStartTime = (Just __currentTime)
                    ,
                            userActiveEndTime = Nothing
                    ,
                            userDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_userId <- P.insert (e2 :: User)
        prepareNewUser (authId) (result_userId)
        e4 <- do
    
            return $ UserGroupContent {
                            userGroupContentUserGroupId = (userDefaultUserGroupId __auth)
                    ,
                            userGroupContentFileContentId = Nothing
                    ,
                            userGroupContentUserGroupContentId = Nothing
                    ,
                            userGroupContentUserContentId = (Just result_userId)
                    ,
                            userGroupContentCompanyContentId = Nothing
                    ,
                            userGroupContentDeletedVersionId = Nothing
    
                }
        vErrors <- lift $ validate e4
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e4 :: UserGroupContent)
        return $ A.object [ "id" .= (toJSON result_userId) ]
    return $ runDB_result
