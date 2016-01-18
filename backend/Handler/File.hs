module Handler.File where

import Import hiding ((==.), on, isNothing)
import Handler.DB
import Yesod.Auth
import Data.Int
import Database.Esqueleto
import Data.Time.Clock
import System.IO (hPutStrLn, stderr)
import Network.HTTP.Types (status200, status206, status404, hRange,  HeaderName)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified System.FilePath as FP
import qualified System.Directory as D
import qualified System.Posix as SP
import qualified Network.Wai as W
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)    
import Data.Text.Encoding (encodeUtf8)
getFileR :: FileId -> Handler()
getFileR fileId = do
    authId <- requireAuthId
    results <- runDB $ select $ from $ 
        \(f) -> do
            where_ (just (f ^. FileId) `in_` 
                    (subList_select $ from $ \ugc -> do
                        where_ ((ugc ^. UserGroupContentUserGroupId)  `in_`
                            (subList_select $ from $ (\(ug `InnerJoin` ugi) -> do
                                on (ugi ^. UserGroupItemUserGroupId ==. ug ^. UserGroupId)
                                where_ (ugi ^. UserGroupItemUserId ==. (val authId))
                                where_ $ isNothing $ ugi ^. UserGroupItemDeletedVersionId
                                return (ug ^. UserGroupId))))
                        return $ ugc ^. UserGroupContentFileContentId))
            where_ (f ^. FileId ==. (val fileId))
            return f
    case results of        
        ((Entity _ f):_) -> do
            uploadDir <- fmap (appUploadDir . appSettings) getYesod
            let path = FP.joinPath [ 
                      uploadDir,
                      (T.unpack $ toPathPiece fileId)
                 ]
            sendWaiResponse $ W.responseFile status200 [] path Nothing
        _ -> notFoundError
        where notFoundError = sendResponseStatus status404 $ A.object [ "error" .= ("Requested file not found" :: Text) ]

