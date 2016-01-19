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
data FieldType = FieldTypeFTDay | FieldTypeFTBool | FieldTypeFTEnum | FieldTypeFTInt | FieldTypeFTDouble | FieldTypeFTText deriving (Eq, Ord, Enum)

instance Read FieldType where
    readsPrec _ ('F':'T':'D':'a':'y':xs) = [ (FieldTypeFTDay, xs) ]
    readsPrec _ ('F':'T':'B':'o':'o':'l':xs) = [ (FieldTypeFTBool, xs) ]
    readsPrec _ ('F':'T':'E':'n':'u':'m':xs) = [ (FieldTypeFTEnum, xs) ]
    readsPrec _ ('F':'T':'I':'n':'t':xs) = [ (FieldTypeFTInt, xs) ]
    readsPrec _ ('F':'T':'D':'o':'u':'b':'l':'e':xs) = [ (FieldTypeFTDouble, xs) ]
    readsPrec _ ('F':'T':'T':'e':'x':'t':xs) = [ (FieldTypeFTText, xs) ]
    readsPrec _ _ = [ ]

instance Show FieldType where
    show FieldTypeFTDay = "FTDay"
    show FieldTypeFTBool = "FTBool"
    show FieldTypeFTEnum = "FTEnum"
    show FieldTypeFTInt = "FTInt"
    show FieldTypeFTDouble = "FTDouble"
    show FieldTypeFTText = "FTText"

    
derivePersistField "FieldType"

instance A.FromJSON FieldType where
    parseJSON = A.withText "FieldType" (\v -> case v of
        "FTDay" -> return FieldTypeFTDay
        "FTBool" -> return FieldTypeFTBool
        "FTEnum" -> return FieldTypeFTEnum
        "FTInt" -> return FieldTypeFTInt
        "FTDouble" -> return FieldTypeFTDouble
        "FTText" -> return FieldTypeFTText
        _ -> mzero)

instance A.ToJSON FieldType where
    toJSON FieldTypeFTDay = A.String "FTDay"
    toJSON FieldTypeFTBool = A.String "FTBool"
    toJSON FieldTypeFTEnum = A.String "FTEnum"
    toJSON FieldTypeFTInt = A.String "FTInt"
    toJSON FieldTypeFTDouble = A.String "FTDouble"
    toJSON FieldTypeFTText = A.String "FTText"


instance PP.PathPiece FieldType where
    fromPathPiece "FTDay" = Just FieldTypeFTDay
    fromPathPiece "FTBool" = Just FieldTypeFTBool
    fromPathPiece "FTEnum" = Just FieldTypeFTEnum
    fromPathPiece "FTInt" = Just FieldTypeFTInt
    fromPathPiece "FTDouble" = Just FieldTypeFTDouble
    fromPathPiece "FTText" = Just FieldTypeFTText

    fromPathPiece _ = Nothing
    toPathPiece FieldTypeFTDay = "FTDay"
    toPathPiece FieldTypeFTBool = "FTBool"
    toPathPiece FieldTypeFTEnum = "FTEnum"
    toPathPiece FieldTypeFTInt = "FTInt"
    toPathPiece FieldTypeFTDouble = "FTDouble"
    toPathPiece FieldTypeFTText = "FTText"


instance FieldFilter FieldType where
instance FieldFilter (Maybe FieldType) where
data CFName = CFNameNominationCommittee | CFNameDualClassShares | CFNameListedSite | CFNameSegment | CFNameSector | CFNameCountry | CFNameName deriving (Eq, Ord, Enum)

instance Read CFName where
    readsPrec _ ('N':'o':'m':'i':'n':'a':'t':'i':'o':'n':'C':'o':'m':'m':'i':'t':'t':'e':'e':xs) = [ (CFNameNominationCommittee, xs) ]
    readsPrec _ ('D':'u':'a':'l':'C':'l':'a':'s':'s':'S':'h':'a':'r':'e':'s':xs) = [ (CFNameDualClassShares, xs) ]
    readsPrec _ ('L':'i':'s':'t':'e':'d':'S':'i':'t':'e':xs) = [ (CFNameListedSite, xs) ]
    readsPrec _ ('S':'e':'g':'m':'e':'n':'t':xs) = [ (CFNameSegment, xs) ]
    readsPrec _ ('S':'e':'c':'t':'o':'r':xs) = [ (CFNameSector, xs) ]
    readsPrec _ ('C':'o':'u':'n':'t':'r':'y':xs) = [ (CFNameCountry, xs) ]
    readsPrec _ ('N':'a':'m':'e':xs) = [ (CFNameName, xs) ]
    readsPrec _ _ = [ ]

instance Show CFName where
    show CFNameNominationCommittee = "NominationCommittee"
    show CFNameDualClassShares = "DualClassShares"
    show CFNameListedSite = "ListedSite"
    show CFNameSegment = "Segment"
    show CFNameSector = "Sector"
    show CFNameCountry = "Country"
    show CFNameName = "Name"

    
derivePersistField "CFName"

instance A.FromJSON CFName where
    parseJSON = A.withText "CFName" (\v -> case v of
        "NominationCommittee" -> return CFNameNominationCommittee
        "DualClassShares" -> return CFNameDualClassShares
        "ListedSite" -> return CFNameListedSite
        "Segment" -> return CFNameSegment
        "Sector" -> return CFNameSector
        "Country" -> return CFNameCountry
        "Name" -> return CFNameName
        _ -> mzero)

instance A.ToJSON CFName where
    toJSON CFNameNominationCommittee = A.String "NominationCommittee"
    toJSON CFNameDualClassShares = A.String "DualClassShares"
    toJSON CFNameListedSite = A.String "ListedSite"
    toJSON CFNameSegment = A.String "Segment"
    toJSON CFNameSector = A.String "Sector"
    toJSON CFNameCountry = A.String "Country"
    toJSON CFNameName = A.String "Name"


instance PP.PathPiece CFName where
    fromPathPiece "NominationCommittee" = Just CFNameNominationCommittee
    fromPathPiece "DualClassShares" = Just CFNameDualClassShares
    fromPathPiece "ListedSite" = Just CFNameListedSite
    fromPathPiece "Segment" = Just CFNameSegment
    fromPathPiece "Sector" = Just CFNameSector
    fromPathPiece "Country" = Just CFNameCountry
    fromPathPiece "Name" = Just CFNameName

    fromPathPiece _ = Nothing
    toPathPiece CFNameNominationCommittee = "NominationCommittee"
    toPathPiece CFNameDualClassShares = "DualClassShares"
    toPathPiece CFNameListedSite = "ListedSite"
    toPathPiece CFNameSegment = "Segment"
    toPathPiece CFNameSector = "Sector"
    toPathPiece CFNameCountry = "Country"
    toPathPiece CFNameName = "Name"


instance FieldFilter CFName where
instance FieldFilter (Maybe CFName) where
