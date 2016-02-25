module DB.Shares where
import Prelude
import Data.Text
import Xlsx.Types

data Shares = Shares {
    ownerName       :: Text,
    investorType    :: Text,
    city            :: Text,
    country         :: Text,
    shares          :: Int,
    positionChange  :: Int
} deriving (Show)

