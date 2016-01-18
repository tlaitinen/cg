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
module Handler.DB.RouteUsersUser where
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

getUsersUserIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => UserId -> HandlerT DB (HandlerT master IO) A.Value
getUsersUserIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    let baseQuery limitOffsetOrder = from $ \(u ) -> do
        let uId' = u ^. UserId
        where_ (((u ^. UserId) ==. (val p1)) &&. (hasReadPerm (val authId) (u ^. UserId)))

        _ <- when limitOffsetOrder $ do
            offset 0
            limit 10000
            orderBy [  ]

                 
        return (u ^. UserId, u ^. UserFirstName, u ^. UserLastName, u ^. UserOrganization, u ^. UserAdmin, u ^. UserEmail, u ^. UserContractStartDate, u ^. UserContractEndDate, u ^. UserDefaultUserGroupId, u ^. UserTimeZone, u ^. UserCurrent, u ^. UserConfig, u ^. UserStrictEmailCheck, u ^. UserName)
    count <- select $ do
        baseQuery False
        let countRows' = countRows
        orderBy []
        return $ (countRows' :: SqlExpr (Database.Esqueleto.Value Int))
    results <- select $ baseQuery True
    (return $ A.object [
        "totalCount" .= ((\(Database.Esqueleto.Value v) -> (v::Int)) (head count)),
        "result" .= (toJSON $ map (\row -> case row of
                ((Database.Esqueleto.Value f1), (Database.Esqueleto.Value f2), (Database.Esqueleto.Value f3), (Database.Esqueleto.Value f4), (Database.Esqueleto.Value f5), (Database.Esqueleto.Value f6), (Database.Esqueleto.Value f7), (Database.Esqueleto.Value f8), (Database.Esqueleto.Value f9), (Database.Esqueleto.Value f10), (Database.Esqueleto.Value f11), (Database.Esqueleto.Value f12), (Database.Esqueleto.Value f13), (Database.Esqueleto.Value f14)) -> A.object [
                    "id" .= toJSON f1,
                    "firstName" .= toJSON f2,
                    "lastName" .= toJSON f3,
                    "organization" .= toJSON f4,
                    "admin" .= toJSON f5,
                    "email" .= toJSON f6,
                    "contractStartDate" .= toJSON f7,
                    "contractEndDate" .= toJSON f8,
                    "defaultUserGroupId" .= toJSON f9,
                    "timeZone" .= toJSON f10,
                    "current" .= toJSON f11,
                    "config" .= toJSON f12,
                    "strictEmailCheck" .= toJSON f13,
                    "name" .= toJSON f14                                    
                    ]
                _ -> A.object []
            ) results)
       ])
deleteUsersUserIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => UserId -> HandlerT DB (HandlerT master IO) A.Value
deleteUsersUserIdR p1 = lift $ runDB $ do
    authId <- lift $ requireAuthId
    __currentTime <- liftIO $ getCurrentTime
    _ <- do
        result <- select $ from $ \(u ) -> do
            let uId' = u ^. UserId
            where_ ((((u ^. UserId) ==. (val p1)) &&. ((u ^. UserId) !=. (val authId))) &&. (hasWritePerm (val authId) (u ^. UserId)))

            limit 1
            return u
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        e2 <- do
    
            return $ Version {
                            versionTime = __currentTime
                    ,
                            versionUserId = authId
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_versionId <- P.insert (e2 :: Version)
        e3 <- do
            es <- select $ from $ \o -> do
                where_ (o ^. UserId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing User" :: Text)
                    ]
    
            return $ e {
                            userCurrent = Inactive
                    ,
                            userActiveEndTime = (Just __currentTime)
                    ,
                            userDeletedVersionId = (Just result_versionId)
    
                }
        vErrors <- lift $ validate e3
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e3 :: User)
        return AT.emptyObject
    return $ runDB_result
putUsersUserIdR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => UserId -> HandlerT DB (HandlerT master IO) A.Value
putUsersUserIdR p1 = lift $ runDB $ do
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
    _ <- do
        result <- select $ from $ \(u ) -> do
            let uId' = u ^. UserId
            where_ (((u ^. UserId) ==. (val p1)) &&. (hasWritePerm (val authId) (u ^. UserId)))

            limit 1
            return u
        case result of
            ((Entity _ _):_) -> return ()
            _ -> sendResponseStatus status403 (A.object [
                    "message" .= ("require condition #1 failed" :: Text)
                    ])
    runDB_result <- do
        e2 <- do
    
            return $ Version {
                            versionTime = __currentTime
                    ,
                            versionUserId = authId
    
                }
        vErrors <- lift $ validate e2
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        result_versionId <- P.insert (e2 :: Version)
        result_u <- do
            r <- get $ ((p1) :: UserId)
            case r of
                Just e -> return e
                _ -> sendResponseStatus status400 $ A.object [  
                    "message" .= ("Could not get entity User" :: Text) 
                   ] 
        e4 <- do
            let e = result_u
    
            return $ e {
                            userCurrent = Inactive
                    ,
                            userActiveId = (Just p1)
                    ,
                            userActiveEndTime = (Just __currentTime)
                    ,
                            userDeletedVersionId = (Just result_versionId)
    
                }
        vErrors <- lift $ validate e4
        case vErrors of
            xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                        "message" .= ("Entity validation failed" :: Text),
                        "errors" .= toJSON xs 
                    ])
            _ -> return ()
        P.insert (e4 :: User)
        e5 <- do
            es <- select $ from $ \o -> do
                where_ (o ^. UserId ==. (val p1))
                limit 1
                return o
            e <- case es of
                [(Entity _ e')] -> return e'    
                _ -> sendResponseStatus status404 $ A.object [ 
                        "message" .= ("Could not update a non-existing User" :: Text)
                    ]
    
            return $ e {
                            userFirstName = attr_firstName
                    ,
                            userLastName = attr_lastName
                    ,
                            userOrganization = attr_organization
                    ,
                            userEmail = attr_email
                    ,
                            userDefaultUserGroupId = attr_defaultUserGroupId
                    ,
                            userTimeZone = attr_timeZone
                    ,
                            userStrictEmailCheck = attr_strictEmailCheck
                    ,
                            userName = attr_name
                    ,
                            userActiveStartTime = (Just __currentTime)
    
                }
        vErrors <- lift $ validate e5
        case vErrors of
             xs@(_:_) -> sendResponseStatus status400 (A.object [ 
                         "message" .= ("Entity validation failed" :: Text),
                         "errors" .= toJSON xs 
                     ])
             _ -> P.repsert p1 (e5 :: User)
        return AT.emptyObject
    return $ runDB_result
