module Xlsx.Types where
import Prelude
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

type RowMap      = HM.HashMap Int Value
type ColMap      = HM.HashMap Text Int
type DataSet     = (ColMap, [RowMap])

type DataSetName = Text
type DataSetMap  = HM.HashMap DataSetName DataSet
type QueryResult = [[Value]]  

data Value = VText Text | VDouble Double | VBool Bool 
           deriving (Show, Eq, Ord)


