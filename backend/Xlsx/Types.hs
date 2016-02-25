{-# LANGUAGE InstanceSigs #-}
module Xlsx.Types where
import Prelude
import Codec.Xlsx
import Data.Maybe
import Data.Text
import Data.Text.Read
import Control.Lens

sheetRows :: Worksheet -> [[Maybe CellValue]]
sheetRows ws = [ [ v ^. cellValue | (_,v) <- r ] | (_,r) <- toRows $ ws ^. wsCells ]

class FromCellValue a where
    fromCellValue :: Maybe CellValue -> Maybe a

instance FromCellValue Text where
    fromCellValue :: Maybe CellValue -> Maybe Text
    fromCellValue v = case v of
        Just (CellText t) -> Just t
        _ -> Nothing

instance FromCellValue Int where
    fromCellValue :: Maybe CellValue -> Maybe Int
    fromCellValue v = case v of
        Just (CellDouble d) -> Just $ round d
        Just (CellText t) -> case signed decimal t of
            Right (d,_) -> Just d
            _ -> Nothing
        _ -> Nothing

instance FromCellValue Double where
    fromCellValue :: Maybe CellValue -> Maybe Double
    fromCellValue v = case v of
        Just (CellDouble d) -> Just d
        Just (CellText t) -> case double t of
            Right (d,_) -> Just d
            _ -> Nothing
        _ -> Nothing

instance FromCellValue Bool where
    fromCellValue :: Maybe CellValue -> Maybe Bool
    fromCellValue v = case v of
        Just (CellBool d) -> Just d
        Just (CellDouble d) -> Just (round d == 1)
        _ -> Nothing


