module Xlsx.Convert where
import Prelude
import Codec.Xlsx
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Control.Lens (view)
import Xlsx.Types
dataSetFromRows :: Text -> Text -> [(Int, [(Int, Cell)])] -> DataSet
dataSetFromRows sheetColName sheetName rows = (fromMaybe HM.empty colMap, [ toRowMap r | (_,r) <- rest ])
    where
        firstRow = listToMaybe $ take 1 rows
        rest     = drop 1 rows
        colMap = do   
            (_,fr) <- firstRow
            Just $ HM.fromList 
                 $ [(sheetColName, 0)] ++ catMaybes [ maybeCellText v >>= Just . (,k) 
                                | (k,v) <- fr ]
        toRowMap r = HM.fromList $ [(0,VText sheetName)] ++ catMaybes [ view cellValue v >>= Just . (k,) . fromCellValue
                                                | (k,v) <- r ]


fromCellValue :: CellValue -> Value
fromCellValue cv = case cv of
    CellText t -> VText t
    CellDouble d -> VDouble d
    CellBool b -> VBool b
maybeCellText :: Cell -> Maybe Text        
maybeCellText c = case view cellValue c of
    Just (CellText t) -> Just t
    _ -> Nothing 

