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

fromRow :: [CellValue] -> Maybe Shares
fromRow r = case r of
    on:st:c:co:_:_:s:p:_ -> Shares <$> (fromCellValue' on)
        <*> fromCellValue' st
        <*> fromCellValue' c
        <*> fromCellValue' co
        <*> fromCellValue' s
        <*> fromCellValue' p
    _ -> Nothing

