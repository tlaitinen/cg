{-# LANGUAGE TemplateHaskell #-}
module DB.Board where

import Prelude
import Debug.Trace
import Data.Text (Text)
import Data.Maybe
import Codec.Xlsx
import Control.Lens
import Xlsx.Types
data Gender = Male | Female deriving (Show, Eq)

instance FromCellValue Gender where
    fromCellValue cv = case cv of
        Just (CellText "M") -> Just Male
        Just (CellText "F") -> Just Female
        _ -> Nothing
data BoardMember = BoardMember {
    _year                :: Int,
    _name                :: Text,
    _role                :: Text,
    _type_               :: Text,
    _independent         :: Bool,
    _inExecutiveTeam     :: Bool,
    _roleSinceYear       :: Int,
    _inBoardSinceYear    :: Int,
    _inCompanySinceYear  :: Int,
    _numBoardPos         :: Int,
    _directorAge         :: Int,
    _yearOfBirth         :: Int,
    _gender              :: Gender,
    _nationality         :: Text,
    _education           :: Text,
    _university          :: Text,
    _expertise           :: Text,
    _politician          :: Bool,
    _academic            :: Bool,
    _financial           :: Bool,
    _ceo                 :: Bool,
    _chair               :: Bool,
    _ceoName             :: Text
} deriving (Show)
makeLenses ''BoardMember

fromRow :: RowMap -> Maybe BoardMember
fromRow r = BoardMember 
    <$> (fromRowValue r 1)
    <*> (fromRowValue r 2)
    <*> (fromRowValue r 3)
    <*> (fromRowValue r 4)
    <*> (fromRowValue r 5)
    <*> (fromRowValue r 6)
    <*> (fromRowValue r 8)
    <*> (fromRowValue r 9)
    <*> (fromRowValue r 10)
    <*> (fromRowValue r 11)
    <*> (fromRowValue r 12)
    <*> (fromRowValue r 13)
    <*> (fromRowValue r 14)
    <*> (fromRowValue r 15)
    <*> (fromRowValue r 16)
    <*> (fromRowValue r 17)
    <*> (fromRowValue r 18)
    <*> (fromRowValue r 19)
    <*> (fromRowValue r 20)
    <*> (fromRowValue r 21)
    <*> (fromRowValue r 23)
    <*> (fromRowValue r 24)
    <*> (fromRowValue r 25)

data Board = Board {
    _country             :: Text,
    _sector              :: Text,
    _sicCode             :: Text,
    _company             :: Text,
    _thomsonId           :: Text,
    _isin                :: Text,
    _listedSite          :: Text,
    _dualClassShares     :: Bool,
    _foundedYear         :: Int,
    _ipoYear             :: Int,
    _delistedYear        :: Int,
    _nominationCommittee :: Text,
    _members             :: [BoardMember]
} deriving (Show)
makeLenses ''Board

fromXlsx :: Xlsx -> Maybe Board
fromXlsx x = case maybe [] (take 1 . drop 1 . sheetRows) $ x ^? ixSheet "Firm" of
    a@[ r ] -> trace (show a) $ Board
        <$> fromRowValue r 1
        <*> fromRowValue r 2
        <*> fromRowValue r 3
        <*> fromRowValue r 4
        <*> fromRowValue r 5
        <*> fromRowValue r 6 
        <*> fromRowValue r 7
        <*> fromRowValue r 8 
        <*> fromRowValue r 9
        <*> fromRowValue r 10
        <*> fromRowValue r 11
        <*> fromRowValue r 12
        <*> (Just $ mapMaybe fromRow $ maybe [] (drop 1 . sheetRows) $ x ^? ixSheet "Board")
    b -> trace (show b) Nothing

