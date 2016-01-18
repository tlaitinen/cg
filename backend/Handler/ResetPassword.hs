{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.ResetPassword (postResetPasswordR) where
import Prelude ((!!))
import Yesod.Auth
import Import
import Text.Shakespeare.Text hiding (toText)
import Handler.DB
import Database.Persist.Sql
import Handler.Utils
import Data.Time
import Network.HTTP.Types (status400)
import qualified Data.Aeson as A
import Data.Aeson.TH
import Handler.Utils
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.Mail.SMTP (sendMail)
import Network.Mail.Mime
postResetPasswordR :: Handler ()
postResetPasswordR = do 
    app <- getYesod
    let settings = appSettings app
    memail <- lookupPostParam "email"
    let email = fromMaybe "" memail
    runDB $ do
        mu <- getBy $ UniqueUserEmail Active email
        case mu of
            Just (Entity uId u) -> do 
                    token <- liftIO $ rndString 43
                    now <- liftIO getCurrentTime
                    update uId [
                            UserPasswordResetToken =. Just token,
                            UserPasswordResetValidUntil =. (Just $ addUTCTime 3600 now)
                        ]
                    
                    liftIO $ do
                        (plain, html) <- messageBody $ url settings uId token
                        mail <- simpleMail 
                            (Address 
                                (Just $ T.concat [
                                        userFirstName u, " ", userLastName u
                                    ])
                                email)
                            (Address Nothing $ appSenderEmail settings)
                            (renderMessage app langs MsgResetPasswordEmailTitle)
                            plain html []   
                        sendMail (appSmtpAddress settings) mail
            Nothing -> return ()                        
    where
        url settings uId token = T.concat [
                appRoot settings,
                "/?userId=", T.pack $ show $ fromSqlKey uId,
                "&token=",
                token
            ]
        langs = ["fi"]
        messageBody url = do
            let html = LT.fromStrict $(codegenFile "templates/resetpassword.html")
            let plain = LT.fromStrict $(codegenFile "templates/resetpassword.txt")
            return (plain,html)
        
