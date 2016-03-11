{-# LANGUAGE TemplateHaskell #-}
module DB.Shares where
import Prelude
import Data.Text
import Xlsx.Types
import Codec.Xlsx

data Shares = Shares {
    _ownerName       :: Text,
    _investorType    :: Text,
    _city            :: Text,
    _country         :: Text,
    _shares          :: Int,
    _positionChange  :: Int
} deriving (Show)

fromRow :: RowMap -> Maybe Shares
fromRow r = Shares <$> (fromRowValue r 1)
    <*> (fromRowValue r 2)
    <*> (fromRowValue r 3)
    <*> (fromRowValue r 4)
    <*> (fromRowValue r 7)
    <*> (fromRowValue r 8)

