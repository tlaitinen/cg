module Xlsx.Test where
import Prelude
import Control.Monad
import System.Environment
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Codec.Xlsx
import Control.Lens
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import Xlsx.Types
import Xlsx.Convert
import Data.Text (Text)

xlsxTestMain :: IO ()
xlsxTestMain = do
    args <- getArgs
    forM_ args $ \path -> do
        bs <- L.readFile path
        let sheets = view xlSheets (toXlsx bs)
        forM_ (Map.toList sheets) $ \(name,sheet) -> do
            print name
            let ds = dataSetFromRows "sheet" name $ toRows (view wsCells sheet)
            print ds    
