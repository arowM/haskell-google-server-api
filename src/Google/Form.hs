{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Form

Define data types to represent all of the requests that are sent to the API.
-}
module Google.Form
  ( CalendarEvent(..)
  , GmailSend(..)
  , Account(..)
  , DateTime(..)
  , Email(..)
  , toMail
  , MultipartBody(..)
  , GetFileParams(..)
  , DownloadFileParams(..)
  , Token(..)
  ) where

import Data.Aeson (encode)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Base64 as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Mail.Mime (Address(..), Mail(..), renderAddress, simpleMail)
import Servant.API (MimeRender(..))
import Web.FormUrlEncoded (Form(..), ToForm(toForm))
import Web.Internal.HttpApiData (toQueryParam)
import Web.HttpApiData (ToHttpApiData(..))
import qualified Data.HashMap.Strict as HashMap

import Google.Type
  ( ConversionFormat
  , FileId
  , MediaContent(..)
  , MediaType(..)
  , Metadata
  , Multipart
  , Order
  , QueryString
  )


data Account = Account
  { email :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Account

instance IsString Account where
  fromString = Account . fromString

newtype DateTime = DateTime
  { dateTime :: UTCTime
  } deriving (Eq, Generic, Show, Typeable, ToHttpApiData)

deriveJSON defaultOptions ''DateTime

data CalendarEvent = CalendarEvent
  { creator :: Account
  , attendees :: [Account]
  , summary :: Text
  , description :: Text
  , start :: DateTime
  , end :: DateTime
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''CalendarEvent

data Token = Token
  { grantType :: Text
  , assertion :: Text
  } deriving (Eq, Generic, Show, Typeable)

instance ToForm Token where
  toForm token =
    Form . HashMap.fromList $
    [ ("grant_type", [toQueryParam (grantType token)])
    , ("assertion", [toQueryParam (assertion token)])
    ]

data Email = Email
  { to :: Text
  , from :: Text
  , replyTo :: Maybe Text
  , ccs :: [Text]
  , subject :: Text
  , body :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Email

toMail :: Email -> IO Mail
toMail Email {..} = do
  mail <-
    simpleMail
      (Address Nothing to)
      (Address Nothing from)
      subject
      body'
      body'
      []
  pure $
    mail
      { mailHeaders =
          mailHeaders mail <> do
            rt <- maybeToList replyTo
            pure ("Reply-To", renderAddress $ Address Nothing rt)
      , mailCc = map (Address Nothing) ccs
      }
  where
    body' = fromStrict body

data GmailSend = GmailSend
  { raw :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''GmailSend


data GetFileParams = GetFileParams
  { query :: Maybe QueryString
  , orderBy :: Maybe [Order]
  } deriving (Eq, Generic, Show, Typeable)


data MultipartBody = MultipartBody
  { metadata :: Metadata
  , mediaType :: MediaType
  , mediaContent :: MediaContent
  } deriving (Eq, Generic, Show, Typeable)

instance MimeRender Multipart MultipartBody where
  mimeRender _ MultipartBody{..} =
    mconcat
      [ "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: application/json; charset=UTF-8"
      , "\r\n\r\n"
      , encode metadata
      , "\r\n--" <> boundary <> "\r\n"
      , "Content-Type: " <> (LBS.fromStrict $ encodeUtf8 $ mediaTypeName mediaType)
      , "\r\n"
      , "Content-Transfer-Encoding: base64"
      , "\r\n\r\n"
      , LBS.fromStrict $ BSB.encode $ (content mediaContent)
      , "\r\n--" <> boundary <> "--"
      ]
    where
      boundary = "314159265358979323846"


data DownloadFileParams = DownloadFileParams
  { fileId :: FileId
  , conversionFormat :: ConversionFormat
  } deriving (Eq, Generic, Show, Typeable)
