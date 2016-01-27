import Prelude     
import Application (appMain)
import System.Environment
import Xlsx.Test

main :: IO ()
main = do
    progName <- getProgName
    case progName of
        "cg" -> appMain
        "xlsx-test" -> xlsxTestMain
        x -> error $ "Unknown application: " ++ x
