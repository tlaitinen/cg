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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.DB.Internal where
import Handler.DB.Enums
import Handler.DB.Esqueleto
import qualified Handler.DB.PathPieces as PP
import Prelude
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Database.Esqueleto
import qualified Database.Esqueleto as E
import qualified Database.Persist as P
import Database.Persist.TH
import Yesod.Auth (requireAuth, requireAuthId, YesodAuth, AuthId, YesodAuthPersist)
import Yesod.Core hiding (fileName, fileContentType)
import Yesod.Persist (runDB, YesodPersist, YesodPersistBackend)
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
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.List as DL
import Control.Monad (mzero)
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
import qualified Data.Text.Lazy.Builder as TLB
data DB = DB


share [mkPersist sqlSettings, mkMigrate "migrateDB" ] [persistLowerCase|
File json
    contentType Text  
    size Int32  
    name Text  
    activeId FileId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    insertionTime UTCTime  
    insertedByUserId UserId Maybe   default=NULL
UserGroupContent json
    userGroupId UserGroupId  
    fileContentId FileId Maybe   default=NULL
    userGroupContentId UserGroupId Maybe   default=NULL
    userContentId UserId Maybe   default=NULL
    companyContentId CompanyId Maybe   default=NULL
    deletedVersionId VersionId Maybe   default=NULL
UserGroup json
    email Text  "default=''"
    organization Text Maybe  
    current Checkmark  "default=True" nullable
    name Text  
    activeId UserGroupId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUserGroup current name !force
UserGroupItem json
    userGroupId UserGroupId  
    userId UserId  
    mode UserGroupMode  
    deletedVersionId VersionId Maybe   default=NULL
User json
    firstName Text  "default=''"
    lastName Text  "default=''"
    organization Text  "default=''"
    admin Bool  "default=False"
    email Text  "default=''"
    password Text  "default=''"
    salt Text  "default=''"
    passwordResetToken Text Maybe  
    passwordResetValidUntil UTCTime Maybe  
    contractStartDate Day Maybe  
    contractEndDate Day Maybe  
    defaultUserGroupId UserGroupId  
    timeZone Text  "default='Europe/Helsinki'"
    current Checkmark  "default=True" nullable
    config Text  "default='{}'"
    strictEmailCheck Bool  "default=False"
    name Text  
    activeId UserId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    UniqueUser current name !force
    UniqueUserEmail current email !force
    deriving Typeable
Version json
    time UTCTime  
    userId UserId  
EnumType json
    name Text  
EnumValue json
    value Text  
    name Text  
Company json
    isinCode Text  
    dateFounded Day  
    ipoDate Day  
    delistedDate Day  
    activeId CompanyId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
CompanyField json
    name CFName  
    activeId CompanyFieldId Maybe   default=NULL
    activeStartTime UTCTime Maybe  
    activeEndTime UTCTime Maybe  
    deletedVersionId VersionId Maybe   default=NULL
    startDay Day Maybe  
    endDay Day Maybe  
    type FieldType  
    textValue Text Maybe  
    doubleValue Double Maybe  
    intValue Int Maybe  
    enumTypeId EnumTypeId Maybe   default=NULL
    enumValueId EnumValueId Maybe   default=NULL
    boolValue Bool Maybe  
    dayValue Day Maybe  
|]
newFile :: Text -> Int32 -> Text -> UTCTime -> File
newFile contentType_ size_ name_ insertionTime_ = File {
    fileContentType = contentType_,
    fileSize = size_,
    fileName = name_,
    fileActiveId = Nothing,
    fileActiveStartTime = Nothing,
    fileActiveEndTime = Nothing,
    fileDeletedVersionId = Nothing,
    fileInsertionTime = insertionTime_,
    fileInsertedByUserId = Nothing
}    
newUserGroupContent :: UserGroupId -> UserGroupContent
newUserGroupContent userGroupId_ = UserGroupContent {
    userGroupContentUserGroupId = userGroupId_,
    userGroupContentFileContentId = Nothing,
    userGroupContentUserGroupContentId = Nothing,
    userGroupContentUserContentId = Nothing,
    userGroupContentCompanyContentId = Nothing,
    userGroupContentDeletedVersionId = Nothing
}    
newUserGroup :: Text -> UserGroup
newUserGroup name_ = UserGroup {
    userGroupEmail = "",
    userGroupOrganization = Nothing,
    userGroupCurrent = Active,
    userGroupName = name_,
    userGroupActiveId = Nothing,
    userGroupActiveStartTime = Nothing,
    userGroupActiveEndTime = Nothing,
    userGroupDeletedVersionId = Nothing
}    
newUserGroupItem :: UserGroupId -> UserId -> UserGroupMode -> UserGroupItem
newUserGroupItem userGroupId_ userId_ mode_ = UserGroupItem {
    userGroupItemUserGroupId = userGroupId_,
    userGroupItemUserId = userId_,
    userGroupItemMode = mode_,
    userGroupItemDeletedVersionId = Nothing
}    
newUser :: UserGroupId -> Text -> User
newUser defaultUserGroupId_ name_ = User {
    userFirstName = "",
    userLastName = "",
    userOrganization = "",
    userAdmin = False,
    userEmail = "",
    userPassword = "",
    userSalt = "",
    userPasswordResetToken = Nothing,
    userPasswordResetValidUntil = Nothing,
    userContractStartDate = Nothing,
    userContractEndDate = Nothing,
    userDefaultUserGroupId = defaultUserGroupId_,
    userTimeZone = "Europe/Helsinki",
    userCurrent = Active,
    userConfig = "{}",
    userStrictEmailCheck = False,
    userName = name_,
    userActiveId = Nothing,
    userActiveStartTime = Nothing,
    userActiveEndTime = Nothing,
    userDeletedVersionId = Nothing
}    
newVersion :: UTCTime -> UserId -> Version
newVersion time_ userId_ = Version {
    versionTime = time_,
    versionUserId = userId_
}    
newEnumType :: Text -> EnumType
newEnumType name_ = EnumType {
    enumTypeName = name_
}    
newEnumValue :: Text -> Text -> EnumValue
newEnumValue value_ name_ = EnumValue {
    enumValueValue = value_,
    enumValueName = name_
}    
newCompany :: Text -> Day -> Day -> Day -> Company
newCompany isinCode_ dateFounded_ ipoDate_ delistedDate_ = Company {
    companyIsinCode = isinCode_,
    companyDateFounded = dateFounded_,
    companyIpoDate = ipoDate_,
    companyDelistedDate = delistedDate_,
    companyActiveId = Nothing,
    companyActiveStartTime = Nothing,
    companyActiveEndTime = Nothing,
    companyDeletedVersionId = Nothing
}    
newCompanyField :: CFName -> FieldType -> CompanyField
newCompanyField name_ type_ = CompanyField {
    companyFieldName = name_,
    companyFieldActiveId = Nothing,
    companyFieldActiveStartTime = Nothing,
    companyFieldActiveEndTime = Nothing,
    companyFieldDeletedVersionId = Nothing,
    companyFieldStartDay = Nothing,
    companyFieldEndDay = Nothing,
    companyFieldType = type_,
    companyFieldTextValue = Nothing,
    companyFieldDoubleValue = Nothing,
    companyFieldIntValue = Nothing,
    companyFieldEnumTypeId = Nothing,
    companyFieldEnumValueId = Nothing,
    companyFieldBoolValue = Nothing,
    companyFieldDayValue = Nothing
}    
class Named a where
    namedName :: a -> Text
data NamedInstanceFieldName = NamedName 
instance Named File where
    namedName = fileName
instance Named UserGroup where
    namedName = userGroupName
instance Named User where
    namedName = userName
instance Named EnumType where
    namedName = enumTypeName
instance Named EnumValue where
    namedName = enumValueName
data NamedInstance = NamedInstanceFile (Entity File)
    | NamedInstanceUserGroup (Entity UserGroup)
    | NamedInstanceUser (Entity User)
    | NamedInstanceEnumType (Entity EnumType)
    | NamedInstanceEnumValue (Entity EnumValue)


data NamedInstanceId = NamedInstanceFileId FileId
    | NamedInstanceUserGroupId UserGroupId
    | NamedInstanceUserId UserId
    | NamedInstanceEnumTypeId EnumTypeId
    | NamedInstanceEnumValueId EnumValueId
    deriving (Eq, Ord)

reflectNamedInstanceId :: NamedInstanceId -> (Text, Int64)
reflectNamedInstanceId x = case x of
    NamedInstanceFileId key -> ("File", fromSqlKey key)
    NamedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    NamedInstanceUserId key -> ("User", fromSqlKey key)
    NamedInstanceEnumTypeId key -> ("EnumType", fromSqlKey key)
    NamedInstanceEnumValueId key -> ("EnumValue", fromSqlKey key)


instance Named NamedInstance where
    namedName x = case x of
        NamedInstanceFile (Entity _ e) -> fileName e
        NamedInstanceUserGroup (Entity _ e) -> userGroupName e
        NamedInstanceUser (Entity _ e) -> userName e
        NamedInstanceEnumType (Entity _ e) -> enumTypeName e
        NamedInstanceEnumValue (Entity _ e) -> enumValueName e
    
data NamedInstanceFilterType = NamedInstanceNameFilter (SqlExpr (Database.Esqueleto.Value (Text)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupNamedInstance :: forall (m :: * -> *). (MonadIO m) =>
    NamedInstanceId -> SqlPersistT m (Maybe NamedInstance)
lookupNamedInstance k = case k of
        NamedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceFile $ Entity key val
        NamedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceUserGroup $ Entity key val
        NamedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceUser $ Entity key val
        NamedInstanceEnumTypeId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceEnumType $ Entity key val
        NamedInstanceEnumValueId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ NamedInstanceEnumValue $ Entity key val

    
selectNamed :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[NamedInstanceFilterType]] -> SqlPersistT m [NamedInstance]
selectNamed filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. FileName
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserGroupName
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserName
    
            ) exprs
    
        return e
    result_EnumType <- select $ from $ \e -> do
        let _ = e ^. EnumTypeId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. EnumTypeName
    
            ) exprs
    
        return e
    result_EnumValue <- select $ from $ \e -> do
        let _ = e ^. EnumValueId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. EnumValueName
    
            ) exprs
    
        return e

    return $ concat [
        map NamedInstanceFile result_File
        , map NamedInstanceUserGroup result_UserGroup
        , map NamedInstanceUser result_User
        , map NamedInstanceEnumType result_EnumType
        , map NamedInstanceEnumValue result_EnumValue

        ]
data NamedInstanceUpdateType = NamedInstanceUpdateName (SqlExpr (Database.Esqueleto.Value (Text)))
updateNamed :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[NamedInstanceFilterType]] -> [NamedInstanceUpdateType] -> SqlPersistT m ()
updateNamed filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> FileName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. FileName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> UserGroupName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserGroupName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> UserName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. UserName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. EnumTypeId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> EnumTypeName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. EnumTypeName
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. EnumValueId
        set e $ map (\u -> case u of
                    NamedInstanceUpdateName v -> EnumValueName =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                NamedInstanceNameFilter op -> op $ e ^. EnumValueName
    
            ) exprs
    
     
                

    return ()

class HasInsertInfo a where
    hasInsertInfoInsertionTime :: a -> UTCTime
    hasInsertInfoInsertedByUserId :: a -> Maybe UserId
data HasInsertInfoInstanceFieldName = HasInsertInfoInsertionTime    | HasInsertInfoInsertedByUserId 
instance HasInsertInfo File where
    hasInsertInfoInsertionTime = fileInsertionTime
    hasInsertInfoInsertedByUserId = fileInsertedByUserId
data HasInsertInfoInstance = HasInsertInfoInstanceFile (Entity File)


data HasInsertInfoInstanceId = HasInsertInfoInstanceFileId FileId
    deriving (Eq, Ord)

reflectHasInsertInfoInstanceId :: HasInsertInfoInstanceId -> (Text, Int64)
reflectHasInsertInfoInstanceId x = case x of
    HasInsertInfoInstanceFileId key -> ("File", fromSqlKey key)


instance HasInsertInfo HasInsertInfoInstance where
    hasInsertInfoInsertionTime x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertionTime e
    
    hasInsertInfoInsertedByUserId x = case x of
        HasInsertInfoInstanceFile (Entity _ e) -> fileInsertedByUserId e
    
data HasInsertInfoInstanceFilterType = HasInsertInfoInstanceInsertionTimeFilter (SqlExpr (Database.Esqueleto.Value (UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | HasInsertInfoInstanceInsertedByUserIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe UserId)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupHasInsertInfoInstance :: forall (m :: * -> *). (MonadIO m) =>
    HasInsertInfoInstanceId -> SqlPersistT m (Maybe HasInsertInfoInstance)
lookupHasInsertInfoInstance k = case k of
        HasInsertInfoInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ HasInsertInfoInstanceFile $ Entity key val

    
selectHasInsertInfo :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasInsertInfoInstanceFilterType]] -> SqlPersistT m [HasInsertInfoInstance]
selectHasInsertInfo filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. FileInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. FileInsertedByUserId
    
            ) exprs
    
        return e

    return $ concat [
        map HasInsertInfoInstanceFile result_File

        ]
data HasInsertInfoInstanceUpdateType = HasInsertInfoInstanceUpdateInsertionTime (SqlExpr (Database.Esqueleto.Value (UTCTime)))    | HasInsertInfoInstanceUpdateInsertedByUserId (SqlExpr (Database.Esqueleto.Value (Maybe UserId)))
updateHasInsertInfo :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasInsertInfoInstanceFilterType]] -> [HasInsertInfoInstanceUpdateType] -> SqlPersistT m ()
updateHasInsertInfo filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    HasInsertInfoInstanceUpdateInsertionTime v -> FileInsertionTime =. v
                    HasInsertInfoInstanceUpdateInsertedByUserId v -> FileInsertedByUserId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasInsertInfoInstanceInsertionTimeFilter op -> op $ e ^. FileInsertionTime
                HasInsertInfoInstanceInsertedByUserIdFilter op -> op $ e ^. FileInsertedByUserId
    
            ) exprs
    
     
                

    return ()

class Restricted a where
instance Restricted File where
instance Restricted UserGroup where
instance Restricted User where
instance Restricted Company where
data RestrictedInstance = RestrictedInstanceFile (Entity File)
    | RestrictedInstanceUserGroup (Entity UserGroup)
    | RestrictedInstanceUser (Entity User)
    | RestrictedInstanceCompany (Entity Company)


data RestrictedInstanceId = RestrictedInstanceFileId FileId
    | RestrictedInstanceUserGroupId UserGroupId
    | RestrictedInstanceUserId UserId
    | RestrictedInstanceCompanyId CompanyId
    deriving (Eq, Ord)

reflectRestrictedInstanceId :: RestrictedInstanceId -> (Text, Int64)
reflectRestrictedInstanceId x = case x of
    RestrictedInstanceFileId key -> ("File", fromSqlKey key)
    RestrictedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    RestrictedInstanceUserId key -> ("User", fromSqlKey key)
    RestrictedInstanceCompanyId key -> ("Company", fromSqlKey key)


instance Restricted RestrictedInstance where
lookupRestrictedInstance :: forall (m :: * -> *). (MonadIO m) =>
    RestrictedInstanceId -> SqlPersistT m (Maybe RestrictedInstance)
lookupRestrictedInstance k = case k of
        RestrictedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceFile $ Entity key val
        RestrictedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceUserGroup $ Entity key val
        RestrictedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceUser $ Entity key val
        RestrictedInstanceCompanyId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ RestrictedInstanceCompany $ Entity key val

    
selectRestricted :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
     SqlPersistT m [RestrictedInstance]
selectRestricted  = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
    
        return e
    result_Company <- select $ from $ \e -> do
        let _ = e ^. CompanyId
    
        return e

    return $ concat [
        map RestrictedInstanceFile result_File
        , map RestrictedInstanceUserGroup result_UserGroup
        , map RestrictedInstanceUser result_User
        , map RestrictedInstanceCompany result_Company

        ]
class Versioned a where
    versionedActiveId :: a -> Maybe VersionedInstanceId
    versionedActiveStartTime :: a -> Maybe UTCTime
    versionedActiveEndTime :: a -> Maybe UTCTime
data VersionedInstanceFieldName = VersionedActiveId    | VersionedActiveStartTime    | VersionedActiveEndTime 
instance Versioned File where
    versionedActiveId = (fmap VersionedInstanceFileId) . fileActiveId
    versionedActiveStartTime = fileActiveStartTime
    versionedActiveEndTime = fileActiveEndTime
instance Versioned UserGroup where
    versionedActiveId = (fmap VersionedInstanceUserGroupId) . userGroupActiveId
    versionedActiveStartTime = userGroupActiveStartTime
    versionedActiveEndTime = userGroupActiveEndTime
instance Versioned User where
    versionedActiveId = (fmap VersionedInstanceUserId) . userActiveId
    versionedActiveStartTime = userActiveStartTime
    versionedActiveEndTime = userActiveEndTime
instance Versioned Company where
    versionedActiveId = (fmap VersionedInstanceCompanyId) . companyActiveId
    versionedActiveStartTime = companyActiveStartTime
    versionedActiveEndTime = companyActiveEndTime
instance Versioned CompanyField where
    versionedActiveId = (fmap VersionedInstanceCompanyFieldId) . companyFieldActiveId
    versionedActiveStartTime = companyFieldActiveStartTime
    versionedActiveEndTime = companyFieldActiveEndTime
data VersionedInstance = VersionedInstanceFile (Entity File)
    | VersionedInstanceUserGroup (Entity UserGroup)
    | VersionedInstanceUser (Entity User)
    | VersionedInstanceCompany (Entity Company)
    | VersionedInstanceCompanyField (Entity CompanyField)


data VersionedInstanceId = VersionedInstanceFileId FileId
    | VersionedInstanceUserGroupId UserGroupId
    | VersionedInstanceUserId UserId
    | VersionedInstanceCompanyId CompanyId
    | VersionedInstanceCompanyFieldId CompanyFieldId
    deriving (Eq, Ord)

reflectVersionedInstanceId :: VersionedInstanceId -> (Text, Int64)
reflectVersionedInstanceId x = case x of
    VersionedInstanceFileId key -> ("File", fromSqlKey key)
    VersionedInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    VersionedInstanceUserId key -> ("User", fromSqlKey key)
    VersionedInstanceCompanyId key -> ("Company", fromSqlKey key)
    VersionedInstanceCompanyFieldId key -> ("CompanyField", fromSqlKey key)


instance Versioned VersionedInstance where
    versionedActiveId x = case x of
        VersionedInstanceFile (Entity _ e) -> (fmap VersionedInstanceFileId) $ fileActiveId e
        VersionedInstanceUserGroup (Entity _ e) -> (fmap VersionedInstanceUserGroupId) $ userGroupActiveId e
        VersionedInstanceUser (Entity _ e) -> (fmap VersionedInstanceUserId) $ userActiveId e
        VersionedInstanceCompany (Entity _ e) -> (fmap VersionedInstanceCompanyId) $ companyActiveId e
        VersionedInstanceCompanyField (Entity _ e) -> (fmap VersionedInstanceCompanyFieldId) $ companyFieldActiveId e
    
    versionedActiveStartTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveStartTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveStartTime e
        VersionedInstanceUser (Entity _ e) -> userActiveStartTime e
        VersionedInstanceCompany (Entity _ e) -> companyActiveStartTime e
        VersionedInstanceCompanyField (Entity _ e) -> companyFieldActiveStartTime e
    
    versionedActiveEndTime x = case x of
        VersionedInstanceFile (Entity _ e) -> fileActiveEndTime e
        VersionedInstanceUserGroup (Entity _ e) -> userGroupActiveEndTime e
        VersionedInstanceUser (Entity _ e) -> userActiveEndTime e
        VersionedInstanceCompany (Entity _ e) -> companyActiveEndTime e
        VersionedInstanceCompanyField (Entity _ e) -> companyFieldActiveEndTime e
    
data VersionedInstanceFilterType = VersionedInstanceActiveStartTimeFilter (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))    | VersionedInstanceActiveEndTimeFilter (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupVersionedInstance :: forall (m :: * -> *). (MonadIO m) =>
    VersionedInstanceId -> SqlPersistT m (Maybe VersionedInstance)
lookupVersionedInstance k = case k of
        VersionedInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceFile $ Entity key val
        VersionedInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceUserGroup $ Entity key val
        VersionedInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceUser $ Entity key val
        VersionedInstanceCompanyId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceCompany $ Entity key val
        VersionedInstanceCompanyFieldId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ VersionedInstanceCompanyField $ Entity key val

    
selectVersioned :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[VersionedInstanceFilterType]] -> SqlPersistT m [VersionedInstance]
selectVersioned filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. FileActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. FileActiveEndTime
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserGroupActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserGroupActiveEndTime
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserActiveEndTime
    
            ) exprs
    
        return e
    result_Company <- select $ from $ \e -> do
        let _ = e ^. CompanyId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. CompanyActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. CompanyActiveEndTime
    
            ) exprs
    
        return e
    result_CompanyField <- select $ from $ \e -> do
        let _ = e ^. CompanyFieldId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. CompanyFieldActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. CompanyFieldActiveEndTime
    
            ) exprs
    
        return e

    return $ concat [
        map VersionedInstanceFile result_File
        , map VersionedInstanceUserGroup result_UserGroup
        , map VersionedInstanceUser result_User
        , map VersionedInstanceCompany result_Company
        , map VersionedInstanceCompanyField result_CompanyField

        ]
data VersionedInstanceUpdateType = VersionedInstanceUpdateActiveStartTime (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)))    | VersionedInstanceUpdateActiveEndTime (SqlExpr (Database.Esqueleto.Value (Maybe UTCTime)))
updateVersioned :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[VersionedInstanceFilterType]] -> [VersionedInstanceUpdateType] -> SqlPersistT m ()
updateVersioned filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> FileActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> FileActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. FileActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. FileActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> UserGroupActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> UserGroupActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserGroupActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserGroupActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> UserActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> UserActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. UserActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. UserActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. CompanyId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> CompanyActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> CompanyActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. CompanyActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. CompanyActiveEndTime
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. CompanyFieldId
        set e $ map (\u -> case u of
                    VersionedInstanceUpdateActiveStartTime v -> CompanyFieldActiveStartTime =. v
                    VersionedInstanceUpdateActiveEndTime v -> CompanyFieldActiveEndTime =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                VersionedInstanceActiveStartTimeFilter op -> op $ e ^. CompanyFieldActiveStartTime
                VersionedInstanceActiveEndTimeFilter op -> op $ e ^. CompanyFieldActiveEndTime
    
            ) exprs
    
     
                

    return ()

class Deletable a where
    deletableDeletedVersionId :: a -> Maybe VersionId
data DeletableInstanceFieldName = DeletableDeletedVersionId 
instance Deletable File where
    deletableDeletedVersionId = fileDeletedVersionId
instance Deletable UserGroupContent where
    deletableDeletedVersionId = userGroupContentDeletedVersionId
instance Deletable UserGroup where
    deletableDeletedVersionId = userGroupDeletedVersionId
instance Deletable UserGroupItem where
    deletableDeletedVersionId = userGroupItemDeletedVersionId
instance Deletable User where
    deletableDeletedVersionId = userDeletedVersionId
instance Deletable Company where
    deletableDeletedVersionId = companyDeletedVersionId
instance Deletable CompanyField where
    deletableDeletedVersionId = companyFieldDeletedVersionId
data DeletableInstance = DeletableInstanceFile (Entity File)
    | DeletableInstanceUserGroupContent (Entity UserGroupContent)
    | DeletableInstanceUserGroup (Entity UserGroup)
    | DeletableInstanceUserGroupItem (Entity UserGroupItem)
    | DeletableInstanceUser (Entity User)
    | DeletableInstanceCompany (Entity Company)
    | DeletableInstanceCompanyField (Entity CompanyField)


data DeletableInstanceId = DeletableInstanceFileId FileId
    | DeletableInstanceUserGroupContentId UserGroupContentId
    | DeletableInstanceUserGroupId UserGroupId
    | DeletableInstanceUserGroupItemId UserGroupItemId
    | DeletableInstanceUserId UserId
    | DeletableInstanceCompanyId CompanyId
    | DeletableInstanceCompanyFieldId CompanyFieldId
    deriving (Eq, Ord)

reflectDeletableInstanceId :: DeletableInstanceId -> (Text, Int64)
reflectDeletableInstanceId x = case x of
    DeletableInstanceFileId key -> ("File", fromSqlKey key)
    DeletableInstanceUserGroupContentId key -> ("UserGroupContent", fromSqlKey key)
    DeletableInstanceUserGroupId key -> ("UserGroup", fromSqlKey key)
    DeletableInstanceUserGroupItemId key -> ("UserGroupItem", fromSqlKey key)
    DeletableInstanceUserId key -> ("User", fromSqlKey key)
    DeletableInstanceCompanyId key -> ("Company", fromSqlKey key)
    DeletableInstanceCompanyFieldId key -> ("CompanyField", fromSqlKey key)


instance Deletable DeletableInstance where
    deletableDeletedVersionId x = case x of
        DeletableInstanceFile (Entity _ e) -> fileDeletedVersionId e
        DeletableInstanceUserGroupContent (Entity _ e) -> userGroupContentDeletedVersionId e
        DeletableInstanceUserGroup (Entity _ e) -> userGroupDeletedVersionId e
        DeletableInstanceUserGroupItem (Entity _ e) -> userGroupItemDeletedVersionId e
        DeletableInstanceUser (Entity _ e) -> userDeletedVersionId e
        DeletableInstanceCompany (Entity _ e) -> companyDeletedVersionId e
        DeletableInstanceCompanyField (Entity _ e) -> companyFieldDeletedVersionId e
    
data DeletableInstanceFilterType = DeletableInstanceDeletedVersionIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupDeletableInstance :: forall (m :: * -> *). (MonadIO m) =>
    DeletableInstanceId -> SqlPersistT m (Maybe DeletableInstance)
lookupDeletableInstance k = case k of
        DeletableInstanceFileId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceFile $ Entity key val
        DeletableInstanceUserGroupContentId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroupContent $ Entity key val
        DeletableInstanceUserGroupId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroup $ Entity key val
        DeletableInstanceUserGroupItemId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUserGroupItem $ Entity key val
        DeletableInstanceUserId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceUser $ Entity key val
        DeletableInstanceCompanyId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceCompany $ Entity key val
        DeletableInstanceCompanyFieldId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ DeletableInstanceCompanyField $ Entity key val

    
selectDeletable :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[DeletableInstanceFilterType]] -> SqlPersistT m [DeletableInstance]
selectDeletable filters = do
    result_File <- select $ from $ \e -> do
        let _ = e ^. FileId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. FileDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroupContent <- select $ from $ \e -> do
        let _ = e ^. UserGroupContentId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupContentDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroup <- select $ from $ \e -> do
        let _ = e ^. UserGroupId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupDeletedVersionId
    
            ) exprs
    
        return e
    result_UserGroupItem <- select $ from $ \e -> do
        let _ = e ^. UserGroupItemId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupItemDeletedVersionId
    
            ) exprs
    
        return e
    result_User <- select $ from $ \e -> do
        let _ = e ^. UserId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserDeletedVersionId
    
            ) exprs
    
        return e
    result_Company <- select $ from $ \e -> do
        let _ = e ^. CompanyId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. CompanyDeletedVersionId
    
            ) exprs
    
        return e
    result_CompanyField <- select $ from $ \e -> do
        let _ = e ^. CompanyFieldId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. CompanyFieldDeletedVersionId
    
            ) exprs
    
        return e

    return $ concat [
        map DeletableInstanceFile result_File
        , map DeletableInstanceUserGroupContent result_UserGroupContent
        , map DeletableInstanceUserGroup result_UserGroup
        , map DeletableInstanceUserGroupItem result_UserGroupItem
        , map DeletableInstanceUser result_User
        , map DeletableInstanceCompany result_Company
        , map DeletableInstanceCompanyField result_CompanyField

        ]
data DeletableInstanceUpdateType = DeletableInstanceUpdateDeletedVersionId (SqlExpr (Database.Esqueleto.Value (Maybe VersionId)))
updateDeletable :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[DeletableInstanceFilterType]] -> [DeletableInstanceUpdateType] -> SqlPersistT m ()
updateDeletable filters updates = do
    update $ \e -> do
        let _ = e ^. FileId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> FileDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. FileDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupContentId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupContentDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupContentDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserGroupItemId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserGroupItemDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserGroupItemDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. UserId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> UserDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. UserDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. CompanyId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> CompanyDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. CompanyDeletedVersionId
    
            ) exprs
    
     
                
    update $ \e -> do
        let _ = e ^. CompanyFieldId
        set e $ map (\u -> case u of
                    DeletableInstanceUpdateDeletedVersionId v -> CompanyFieldDeletedVersionId =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                DeletableInstanceDeletedVersionIdFilter op -> op $ e ^. CompanyFieldDeletedVersionId
    
            ) exprs
    
     
                

    return ()

class HasDateRange a where
    hasDateRangeStartDay :: a -> Maybe Day
    hasDateRangeEndDay :: a -> Maybe Day
data HasDateRangeInstanceFieldName = HasDateRangeStartDay    | HasDateRangeEndDay 
instance HasDateRange CompanyField where
    hasDateRangeStartDay = companyFieldStartDay
    hasDateRangeEndDay = companyFieldEndDay
data HasDateRangeInstance = HasDateRangeInstanceCompanyField (Entity CompanyField)


data HasDateRangeInstanceId = HasDateRangeInstanceCompanyFieldId CompanyFieldId
    deriving (Eq, Ord)

reflectHasDateRangeInstanceId :: HasDateRangeInstanceId -> (Text, Int64)
reflectHasDateRangeInstanceId x = case x of
    HasDateRangeInstanceCompanyFieldId key -> ("CompanyField", fromSqlKey key)


instance HasDateRange HasDateRangeInstance where
    hasDateRangeStartDay x = case x of
        HasDateRangeInstanceCompanyField (Entity _ e) -> companyFieldStartDay e
    
    hasDateRangeEndDay x = case x of
        HasDateRangeInstanceCompanyField (Entity _ e) -> companyFieldEndDay e
    
data HasDateRangeInstanceFilterType = HasDateRangeInstanceStartDayFilter (SqlExpr (Database.Esqueleto.Value (Maybe Day)) -> SqlExpr (Database.Esqueleto.Value Bool))    | HasDateRangeInstanceEndDayFilter (SqlExpr (Database.Esqueleto.Value (Maybe Day)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupHasDateRangeInstance :: forall (m :: * -> *). (MonadIO m) =>
    HasDateRangeInstanceId -> SqlPersistT m (Maybe HasDateRangeInstance)
lookupHasDateRangeInstance k = case k of
        HasDateRangeInstanceCompanyFieldId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ HasDateRangeInstanceCompanyField $ Entity key val

    
selectHasDateRange :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasDateRangeInstanceFilterType]] -> SqlPersistT m [HasDateRangeInstance]
selectHasDateRange filters = do
    result_CompanyField <- select $ from $ \e -> do
        let _ = e ^. CompanyFieldId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasDateRangeInstanceStartDayFilter op -> op $ e ^. CompanyFieldStartDay
                HasDateRangeInstanceEndDayFilter op -> op $ e ^. CompanyFieldEndDay
    
            ) exprs
    
        return e

    return $ concat [
        map HasDateRangeInstanceCompanyField result_CompanyField

        ]
data HasDateRangeInstanceUpdateType = HasDateRangeInstanceUpdateStartDay (SqlExpr (Database.Esqueleto.Value (Maybe Day)))    | HasDateRangeInstanceUpdateEndDay (SqlExpr (Database.Esqueleto.Value (Maybe Day)))
updateHasDateRange :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[HasDateRangeInstanceFilterType]] -> [HasDateRangeInstanceUpdateType] -> SqlPersistT m ()
updateHasDateRange filters updates = do
    update $ \e -> do
        let _ = e ^. CompanyFieldId
        set e $ map (\u -> case u of
                    HasDateRangeInstanceUpdateStartDay v -> CompanyFieldStartDay =. v
                    HasDateRangeInstanceUpdateEndDay v -> CompanyFieldEndDay =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                HasDateRangeInstanceStartDayFilter op -> op $ e ^. CompanyFieldStartDay
                HasDateRangeInstanceEndDayFilter op -> op $ e ^. CompanyFieldEndDay
    
            ) exprs
    
     
                

    return ()

class IsTypedField a where
    isTypedFieldType :: a -> FieldType
    isTypedFieldTextValue :: a -> Maybe Text
    isTypedFieldDoubleValue :: a -> Maybe Double
    isTypedFieldIntValue :: a -> Maybe Int
    isTypedFieldEnumTypeId :: a -> Maybe EnumTypeId
    isTypedFieldEnumValueId :: a -> Maybe EnumValueId
    isTypedFieldBoolValue :: a -> Maybe Bool
    isTypedFieldDayValue :: a -> Maybe Day
data IsTypedFieldInstanceFieldName = IsTypedFieldType    | IsTypedFieldTextValue    | IsTypedFieldDoubleValue    | IsTypedFieldIntValue    | IsTypedFieldEnumTypeId    | IsTypedFieldEnumValueId    | IsTypedFieldBoolValue    | IsTypedFieldDayValue 
instance IsTypedField CompanyField where
    isTypedFieldType = companyFieldType
    isTypedFieldTextValue = companyFieldTextValue
    isTypedFieldDoubleValue = companyFieldDoubleValue
    isTypedFieldIntValue = companyFieldIntValue
    isTypedFieldEnumTypeId = companyFieldEnumTypeId
    isTypedFieldEnumValueId = companyFieldEnumValueId
    isTypedFieldBoolValue = companyFieldBoolValue
    isTypedFieldDayValue = companyFieldDayValue
data IsTypedFieldInstance = IsTypedFieldInstanceCompanyField (Entity CompanyField)


data IsTypedFieldInstanceId = IsTypedFieldInstanceCompanyFieldId CompanyFieldId
    deriving (Eq, Ord)

reflectIsTypedFieldInstanceId :: IsTypedFieldInstanceId -> (Text, Int64)
reflectIsTypedFieldInstanceId x = case x of
    IsTypedFieldInstanceCompanyFieldId key -> ("CompanyField", fromSqlKey key)


instance IsTypedField IsTypedFieldInstance where
    isTypedFieldType x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldType e
    
    isTypedFieldTextValue x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldTextValue e
    
    isTypedFieldDoubleValue x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldDoubleValue e
    
    isTypedFieldIntValue x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldIntValue e
    
    isTypedFieldEnumTypeId x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldEnumTypeId e
    
    isTypedFieldEnumValueId x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldEnumValueId e
    
    isTypedFieldBoolValue x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldBoolValue e
    
    isTypedFieldDayValue x = case x of
        IsTypedFieldInstanceCompanyField (Entity _ e) -> companyFieldDayValue e
    
data IsTypedFieldInstanceFilterType = IsTypedFieldInstanceTypeFilter (SqlExpr (Database.Esqueleto.Value (FieldType)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceTextValueFilter (SqlExpr (Database.Esqueleto.Value (Maybe Text)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceDoubleValueFilter (SqlExpr (Database.Esqueleto.Value (Maybe Double)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceIntValueFilter (SqlExpr (Database.Esqueleto.Value (Maybe Int)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceEnumTypeIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe EnumTypeId)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceEnumValueIdFilter (SqlExpr (Database.Esqueleto.Value (Maybe EnumValueId)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceBoolValueFilter (SqlExpr (Database.Esqueleto.Value (Maybe Bool)) -> SqlExpr (Database.Esqueleto.Value Bool))    | IsTypedFieldInstanceDayValueFilter (SqlExpr (Database.Esqueleto.Value (Maybe Day)) -> SqlExpr (Database.Esqueleto.Value Bool))
lookupIsTypedFieldInstance :: forall (m :: * -> *). (MonadIO m) =>
    IsTypedFieldInstanceId -> SqlPersistT m (Maybe IsTypedFieldInstance)
lookupIsTypedFieldInstance k = case k of
        IsTypedFieldInstanceCompanyFieldId key -> runMaybeT $ do
            val <- MaybeT $ get key
            return $ IsTypedFieldInstanceCompanyField $ Entity key val

    
selectIsTypedField :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[IsTypedFieldInstanceFilterType]] -> SqlPersistT m [IsTypedFieldInstance]
selectIsTypedField filters = do
    result_CompanyField <- select $ from $ \e -> do
        let _ = e ^. CompanyFieldId
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                IsTypedFieldInstanceTypeFilter op -> op $ e ^. CompanyFieldType
                IsTypedFieldInstanceTextValueFilter op -> op $ e ^. CompanyFieldTextValue
                IsTypedFieldInstanceDoubleValueFilter op -> op $ e ^. CompanyFieldDoubleValue
                IsTypedFieldInstanceIntValueFilter op -> op $ e ^. CompanyFieldIntValue
                IsTypedFieldInstanceEnumTypeIdFilter op -> op $ e ^. CompanyFieldEnumTypeId
                IsTypedFieldInstanceEnumValueIdFilter op -> op $ e ^. CompanyFieldEnumValueId
                IsTypedFieldInstanceBoolValueFilter op -> op $ e ^. CompanyFieldBoolValue
                IsTypedFieldInstanceDayValueFilter op -> op $ e ^. CompanyFieldDayValue
    
            ) exprs
    
        return e

    return $ concat [
        map IsTypedFieldInstanceCompanyField result_CompanyField

        ]
data IsTypedFieldInstanceUpdateType = IsTypedFieldInstanceUpdateType (SqlExpr (Database.Esqueleto.Value (FieldType)))    | IsTypedFieldInstanceUpdateTextValue (SqlExpr (Database.Esqueleto.Value (Maybe Text)))    | IsTypedFieldInstanceUpdateDoubleValue (SqlExpr (Database.Esqueleto.Value (Maybe Double)))    | IsTypedFieldInstanceUpdateIntValue (SqlExpr (Database.Esqueleto.Value (Maybe Int)))    | IsTypedFieldInstanceUpdateEnumTypeId (SqlExpr (Database.Esqueleto.Value (Maybe EnumTypeId)))    | IsTypedFieldInstanceUpdateEnumValueId (SqlExpr (Database.Esqueleto.Value (Maybe EnumValueId)))    | IsTypedFieldInstanceUpdateBoolValue (SqlExpr (Database.Esqueleto.Value (Maybe Bool)))    | IsTypedFieldInstanceUpdateDayValue (SqlExpr (Database.Esqueleto.Value (Maybe Day)))
updateIsTypedField :: forall (m :: * -> *). 
    (MonadLogger m, MonadIO m, MonadThrow m, MonadBaseControl IO m) => 
    [[IsTypedFieldInstanceFilterType]] -> [IsTypedFieldInstanceUpdateType] -> SqlPersistT m ()
updateIsTypedField filters updates = do
    update $ \e -> do
        let _ = e ^. CompanyFieldId
        set e $ map (\u -> case u of
                    IsTypedFieldInstanceUpdateType v -> CompanyFieldType =. v
                    IsTypedFieldInstanceUpdateTextValue v -> CompanyFieldTextValue =. v
                    IsTypedFieldInstanceUpdateDoubleValue v -> CompanyFieldDoubleValue =. v
                    IsTypedFieldInstanceUpdateIntValue v -> CompanyFieldIntValue =. v
                    IsTypedFieldInstanceUpdateEnumTypeId v -> CompanyFieldEnumTypeId =. v
                    IsTypedFieldInstanceUpdateEnumValueId v -> CompanyFieldEnumValueId =. v
                    IsTypedFieldInstanceUpdateBoolValue v -> CompanyFieldBoolValue =. v
                    IsTypedFieldInstanceUpdateDayValue v -> CompanyFieldDayValue =. v
    
            ) updates
        forM_ filters $ \exprs -> 
            when (not . null $ exprs) $ where_ $ foldl1 (||.) $ map (\expr -> case expr of 
                IsTypedFieldInstanceTypeFilter op -> op $ e ^. CompanyFieldType
                IsTypedFieldInstanceTextValueFilter op -> op $ e ^. CompanyFieldTextValue
                IsTypedFieldInstanceDoubleValueFilter op -> op $ e ^. CompanyFieldDoubleValue
                IsTypedFieldInstanceIntValueFilter op -> op $ e ^. CompanyFieldIntValue
                IsTypedFieldInstanceEnumTypeIdFilter op -> op $ e ^. CompanyFieldEnumTypeId
                IsTypedFieldInstanceEnumValueIdFilter op -> op $ e ^. CompanyFieldEnumValueId
                IsTypedFieldInstanceBoolValueFilter op -> op $ e ^. CompanyFieldBoolValue
                IsTypedFieldInstanceDayValueFilter op -> op $ e ^. CompanyFieldDayValue
    
            ) exprs
    
     
                

    return ()

userGroupContentContentId :: UserGroupContent -> Maybe (RestrictedInstanceId)
userGroupContentContentId e = listToMaybe $ catMaybes [
        userGroupContentFileContentId e >>= (return . RestrictedInstanceFileId)
        , userGroupContentUserGroupContentId e >>= (return . RestrictedInstanceUserGroupId)
        , userGroupContentUserContentId e >>= (return . RestrictedInstanceUserId)
        , userGroupContentCompanyContentId e >>= (return . RestrictedInstanceCompanyId)

    ]

class UserGroupContentContentIdField e where
    userGroupContentContentIdField :: SqlExpr (Database.Esqueleto.Value (Maybe (Key e))) -> EntityField UserGroupContent (Maybe (Key e)) 

instance UserGroupContentContentIdField Company where
    userGroupContentContentIdField _ = UserGroupContentCompanyContentId
instance UserGroupContentContentIdField User where
    userGroupContentContentIdField _ = UserGroupContentUserContentId
instance UserGroupContentContentIdField UserGroup where
    userGroupContentContentIdField _ = UserGroupContentUserGroupContentId
instance UserGroupContentContentIdField File where
    userGroupContentContentIdField _ = UserGroupContentFileContentId
    

userGroupContentContentIdExprFromString :: Text -> SqlExpr (Entity UserGroupContent) -> Text -> Maybe Text -> Maybe (SqlExpr (E.Value Bool))
userGroupContentContentIdExprFromString "Company" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentCompanyContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentCompanyContentId) nothing
   
userGroupContentContentIdExprFromString "User" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) nothing
   
userGroupContentContentIdExprFromString "UserGroup" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) nothing
   
userGroupContentContentIdExprFromString "File" e op vt = case vt of 
    Just vt' -> PP.fromPathPiece vt' >>= \v -> Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) (val v)
    Nothing -> Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) nothing
   

userGroupContentContentIdExprFromString _ _ _ _ = Nothing

userGroupContentContentIdExpr2FromString :: Text -> SqlExpr (Entity UserGroupContent) -> Text -> SqlExpr (Entity UserGroupContent) -> Maybe (SqlExpr (E.Value Bool))
userGroupContentContentIdExpr2FromString "Company" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentCompanyContentId) (e2 ^. UserGroupContentCompanyContentId)
userGroupContentContentIdExpr2FromString "User" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentUserContentId) (e2 ^. UserGroupContentUserContentId)
userGroupContentContentIdExpr2FromString "UserGroup" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentUserGroupContentId) (e2 ^. UserGroupContentUserGroupContentId)
userGroupContentContentIdExpr2FromString "File" e op e2 = Just $ defaultFilterOp False op (e ^. UserGroupContentFileContentId) (e2 ^. UserGroupContentFileContentId)

userGroupContentContentIdExpr2FromString _ _ _ _ = Nothing


#ifdef ToJSON_Day
instance ToJSON Day where
    toJSON = toJSON . show
#endif

#ifdef FromJSON_Day
instance FromJSON Day where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero 
#endif

instance ToJSON TimeOfDay where
    toJSON = toJSON . show

instance FromJSON TimeOfDay where
    parseJSON x = do
        s <- parseJSON x
        case reads s of
            (d, _):_ -> return d
            [] -> mzero

instance ToJSON Checkmark where
    toJSON Active   = A.String "Active"
    toJSON Inactive = A.String "Inactive"            

instance FromJSON Checkmark where
    parseJSON (A.String "Active") = return Active
    parseJSON (A.String "Inactive") = return Inactive    
    parseJSON _ = mzero   
