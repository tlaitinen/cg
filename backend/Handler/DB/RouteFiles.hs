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
module Handler.DB.RouteFiles where
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

getFilesR :: forall master. (
    YesodAuthPersist master,
    AuthEntity master ~ User,
    AuthId master ~ Key User,
    YesodPersistBackend master ~ SqlBackend)
    => HandlerT DB (HandlerT master IO) A.Value
getFilesR  = lift $ runDB $ do
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
    (filterParam_contentType) <- lookupGetParam "contentType"
    (filterParam_contentTypeList) <- lookupGetParam "contentTypeList"
    (filterParam_hideDeleted :: Maybe Text) <- lookupGetParam "hideDeleted"
    let baseQuery limitOffsetOrder = from $ \(f ) -> do
        let fId' = f ^. FileId
        where_ (hasReadPerm (val authId) (f ^. FileId))

        _ <- when limitOffsetOrder $ do
            offset 0
            limit 10000
            case defaultSortJson of 
                Just xs -> mapM_ (\sjm -> case FS.s_field sjm of
                        "contentType" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (f  ^.  FileContentType) ] 
                            "DESC" -> orderBy [ desc (f  ^.  FileContentType) ] 
                            _      -> return ()
                        "size" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (f  ^.  FileSize) ] 
                            "DESC" -> orderBy [ desc (f  ^.  FileSize) ] 
                            _      -> return ()
                        "name" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (f  ^.  FileName) ] 
                            "DESC" -> orderBy [ desc (f  ^.  FileName) ] 
                            _      -> return ()
                        "insertionTime" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (f  ^.  FileInsertionTime) ] 
                            "DESC" -> orderBy [ desc (f  ^.  FileInsertionTime) ] 
                            _      -> return ()
                        "insertedByUserId" -> case (FS.s_direction sjm) of 
                            "ASC"  -> orderBy [ asc (f  ^.  FileInsertedByUserId) ] 
                            "DESC" -> orderBy [ desc (f  ^.  FileInsertedByUserId) ] 
                            _      -> return ()
            
                        _ -> return ()
                    ) xs
                Nothing -> orderBy [ asc (f ^. FileName) ]

            case defaultOffset of
                Just o -> offset o
                Nothing -> return ()
            case defaultLimit of
                Just l -> limit (min 10000 l)
                Nothing -> return ()
                 
        case defaultFilterJson of 
            Just xs -> mapM_ (\fjm -> case FS.f_field fjm of
                "id" -> case (FS.f_value fjm >>= PP.fromPathPiece)  of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileId) (val v')
                    _        -> return ()
                "contentType" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileContentType) ((val v'))
                    _        -> return ()
                "size" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileSize) ((val v'))
                    _        -> return ()
                "name" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileName) ((val v'))
                    _        -> return ()
                "insertionTime" -> case (FS.f_value fjm >>= PP.fromPathPiece) of 
                    (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertionTime) ((val v'))
                    _        -> return ()
                "insertedByUserId" -> case FS.f_value fjm of
                    Just value -> case PP.fromPathPiece value of 
                            (Just v') -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertedByUserId) (just ((val v')))
                            _        -> return ()
                    Nothing -> where_ $ defaultFilterOp (FS.f_negate fjm) (FS.f_comparison fjm) (f  ^.  FileInsertedByUserId) nothing
                           

                _ -> return ()
                ) xs
            Nothing -> return ()  
        case FS.getDefaultFilter filterParam_query defaultFilterJson "query" of
            Just localParam -> do 
                
                where_ $ (f ^. FileName) `ilike` ((((val "%")) ++. ((val (localParam :: Text)))) ++. ((val "%")))
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_contentType defaultFilterJson "contentType" of
            Just localParam -> do 
                
                where_ $ (f ^. FileContentType) ==. (val localParam)
            Nothing -> return ()
        case FS.getDefaultFilter filterParam_contentTypeList defaultFilterJson "contentTypeList" of
            Just localParam -> do 
                
                where_ $ (f ^. FileContentType) `in_` (valList localParam)
            Nothing -> return ()
        if FS.hasDefaultFilter filterParam_hideDeleted defaultFilterJson "hideDeleted" 
            then do 
                 
                where_ $ (f ^. FileDeletedVersionId) `is` (nothing)
            else return ()
        return (f ^. FileId, f ^. FileContentType, f ^. FileSize, f ^. FileName, f ^. FileInsertionTime, f ^. FileInsertedByUserId)
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
                    "contentType" .= toJSON f2,
                    "size" .= toJSON f3,
                    "name" .= toJSON f4,
                    "insertionTime" .= toJSON f5,
                    "insertedByUserId" .= toJSON f6                                    
                    ]
                _ -> A.object []
            ) results)
       ])
