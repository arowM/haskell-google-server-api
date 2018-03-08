{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Client

Define functions to call Google APIs.
-}
module Google.Client
  ( getToken
  , postCalendarEvent
  , postGmailSend
  , run
  ) where

import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logError)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Base64.URL (encode)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client (HasHttpManager(..), newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Mail.Mime
import Servant.API
  ( (:<|>)(..)
  , (:>)
  , Capture
  , FormUrlEncoded
  , FromHttpApiData
  , Header
  , JSON
  , Post
  , ReqBody
  , ToHttpApiData
  )
import Servant.Client
  ( BaseUrl(BaseUrl)
  , ClientEnv(..)
  , ClientM
  , Scheme(..)
  , ServantError
  , client
  , runClientM
  )

import Google.JWT (JWT)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Google.Form as Form
import qualified Google.JWT as JWT
import qualified Google.Response as Response

newtype Bearer = Bearer
  { _unBearer :: Text
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , FromJSON
             , Generic
             , Ord
             , Show
             , ToHttpApiData
             , ToJSON
             , Typeable
             )

type API
  = "oauth2" :> "v4" :> "token" :>
    ReqBody '[ FormUrlEncoded] Form.Token :>
    Post '[ JSON] Response.Token
  :<|> "calendar" :> "v3" :> "calendars" :>
    Capture "calendarId" Text :>
    "events" :>
    Header "Authorization" Bearer :>
    ReqBody '[ JSON] Form.CalendarEvent :>
    Post '[ JSON] Response.CalendarEvent
  :<|> "gmail" :> "v1" :> "users" :> "me" :> "messages" :> "send" :>
    Header "Authorization" Bearer :>
    ReqBody '[ JSON] Form.GmailSend :>
    Post '[ JSON] Response.GmailSend

api :: Proxy API
api = Proxy

getToken' :: Form.Token -> ClientM Response.Token
postCalendarEvent' ::
     Text
  -> Maybe Bearer
  -> Form.CalendarEvent
  -> ClientM Response.CalendarEvent
postGmailSend' :: Maybe Bearer -> Form.GmailSend -> ClientM Response.GmailSend
getToken' :<|> postCalendarEvent' :<|> postGmailSend' = client api

getToken ::
     (HasHttpManager r, MonadError ServantError m, MonadIO m, MonadReader r m)
  => Maybe JWT.Email
  -> JWT
  -> [JWT.Scope]
  -> m Response.Token
getToken maccount jwt scopes =
  (liftEither =<<) . runExceptT . ExceptT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    Right a <- liftIO $ JWT.getSignedJWT jwt maccount scopes Nothing
    liftIO $
      runClientM
        (getToken' $
         Form.Token
           { grantType = googleGrantType
           , assertion = decodeUtf8 . JWT.unSignedJWT $ a
           })
        (ClientEnv manager googleBaseUrl)

postCalendarEvent ::
     (HasHttpManager r, MonadError ServantError m, MonadIO m, MonadReader r m)
  => Response.Token
  -> Form.CalendarEvent
  -> m Response.CalendarEvent
postCalendarEvent token event =
  runExceptTIO . ExceptT $ do
    manager <- newManager tlsManagerSettings
    runClientM
      (postCalendarEvent'
         (Form.email . Form.creator $ event)
         (pure . toBearer $ token)
         event)
      (ClientEnv manager googleBaseUrl)

postGmailSend ::
     (HasHttpManager r, MonadError ServantError m, MonadIO m, MonadReader r m)
  => Response.Token
  -> Form.Email
  -> m Response.GmailSend
postGmailSend token email =
  runExceptTIO . ExceptT $ do
    manager <- newManager tlsManagerSettings
    mail <- liftIO (renderMail' =<< Form.toMail email)
    let gmailSend =
          Form.GmailSend {raw = decodeUtf8 $ encode $ LBS.toStrict mail}
    T.putStrLn $ "gmailSend: " <> tshow gmailSend
    T.putStrLn $ "from: " <> tshow (Form.to email)
    runClientM
      (postGmailSend' (pure . toBearer $ token) gmailSend)
      (ClientEnv manager googleBaseUrl)

toBearer :: Response.Token -> Bearer
toBearer Response.Token {accessToken} = Bearer $ "Bearer " <> accessToken

run ::
     forall r m a e.
     ( HasHttpManager r
     , MonadIO m
     , MonadError e m
     , MonadLogger m
     , MonadReader r m
     , Show e
     )
  => e
  -> ReaderT r (ExceptT e IO) a
  -> m a
run err m = either doGoogleErr pure =<< run' m
  where
    doGoogleErr :: forall x. e -> m x
    doGoogleErr googleErr = do
      $(logError) $ "Got error response from google API: " <> tshow googleErr
      throwError err

run' ::
     forall r n e a. (HasHttpManager r, MonadIO n, MonadReader r n)
  => ReaderT r (ExceptT e IO) a
  -> n (Either e a)
run' m = do
  r <- ask
  liftIO . runExceptT $ runReaderT m r

{- =================
 -  Constant values
 - ================= -}
googleGrantType :: Text
googleGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"

googleBaseUrl :: BaseUrl
googleBaseUrl = BaseUrl Https "www.googleapis.com" 443 ""

{- =================
 -  Helper functions
 - ================= -}
liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError pure

runExceptTIO :: (MonadError e m, MonadIO m) => ExceptT e IO a -> m a
runExceptTIO = liftEither <=< liftIO . runExceptT

tshow :: Show a => a -> Text
tshow = T.pack . show
