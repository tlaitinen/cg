{-# LANGUAGE TemplateHaskell #-}
module DB.Ownership where
import Prelude
import Data.Text
import DB.Shares
import Codec.Xlsx
import Control.Lens
import Xlsx.Types
import Data.Maybe
import Data.Text.Read 

data ShareType = A | B deriving (Show, Eq, Ord)

data OwnershipSheet = OwnershipSheet {
    _ticker      :: Text,
    _company     :: Text,
    _isin        :: Text,
    _shareType   :: ShareType,
    _year        :: Int,
    _totalShares :: Int,
    _shares      :: [Shares]
} deriving (Show)

makeLenses ''OwnershipSheet

fromWorkSheet :: Text -> Worksheet -> OwnershipSheet
fromWorkSheet sn s = OwnershipSheet {
    _ticker      = read "" (1,1),
    _company     = read "" (1,2),
    _isin        = read "" (1,3),
    _shareType   = case read (""::Text) (1,4) of
        "B" -> B
        _   -> A,
    _year        = read sheetYear (2,1),
    _totalShares = read (0::Int) (2,5),
    _shares      = mapMaybe DB.Shares.fromRow $ sheetRows s
}
    where
        read :: FromCellValue a => a -> (Int,Int) -> a
        read d p = fromMaybe d $ fromCellValue $ s ^? ixCell p . cellValue . _Just
        sheetYear = case signed decimal sn of
            Right (y,_) -> y
            _ -> 0 :: Int


