{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.DB.Enums where
import qualified Handler.DB.PathPieces as PP
import Database.Persist.TH
import qualified Data.Aeson as A
import Prelude
import Control.Monad (mzero)
import Handler.DB.Esqueleto
data UserGroupMode = UserGroupModeReadWrite | UserGroupModeReadOnly deriving (Eq, Ord, Enum)

instance Read UserGroupMode where
    readsPrec _ ('R':'e':'a':'d':'W':'r':'i':'t':'e':xs) = [ (UserGroupModeReadWrite, xs) ]
    readsPrec _ ('R':'e':'a':'d':'O':'n':'l':'y':xs) = [ (UserGroupModeReadOnly, xs) ]
    readsPrec _ _ = [ ]

instance Show UserGroupMode where
    show UserGroupModeReadWrite = "ReadWrite"
    show UserGroupModeReadOnly = "ReadOnly"

    
derivePersistField "UserGroupMode"

instance A.FromJSON UserGroupMode where
    parseJSON = A.withText "UserGroupMode" (\v -> case v of
        "ReadWrite" -> return UserGroupModeReadWrite
        "ReadOnly" -> return UserGroupModeReadOnly
        _ -> mzero)

instance A.ToJSON UserGroupMode where
    toJSON UserGroupModeReadWrite = A.String "ReadWrite"
    toJSON UserGroupModeReadOnly = A.String "ReadOnly"


instance PP.PathPiece UserGroupMode where
    fromPathPiece "ReadWrite" = Just UserGroupModeReadWrite
    fromPathPiece "ReadOnly" = Just UserGroupModeReadOnly

    fromPathPiece _ = Nothing
    toPathPiece UserGroupModeReadWrite = "ReadWrite"
    toPathPiece UserGroupModeReadOnly = "ReadOnly"


instance FieldFilter UserGroupMode where
instance FieldFilter (Maybe UserGroupMode) where
