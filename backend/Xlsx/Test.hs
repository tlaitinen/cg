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
import Data.Text (Text)
import qualified DB.Ownership as O
import qualified DB.Board as B
import DB.Shares

xlsxTestMain :: IO ()
xlsxTestMain = do
    args <- getArgs
    forM_ args $ \path -> do
        bs <- L.readFile path
        let bs' = toXlsx bs
        print $ B.fromXlsx bs'
        let sheets = view xlSheets bs'

        forM_ (Map.toList sheets) $ \(n,s) -> do
            print $ O.fromWorkSheet n s
