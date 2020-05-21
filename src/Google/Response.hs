{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Response

Define data types to represent all of the responses that are received from the Google API.
-}
module Google.Response where

import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (Options(..), defaultOptions, deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Data.Text (intercalate, splitOn)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm, ToForm)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Network.HTTP.Media ((//))
import Servant.API (Accept(..), MimeUnrender(..))
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..), parseUrlPieces, toUrlPieces)

data Token = Token
  { accessToken :: Text
  , tokenType :: Text
  , expiresIn :: Int
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON (defaultOptions {fieldLabelModifier = snakeCase}) ''Token

instance FromForm Token

instance ToForm Token

newtype Account = Account
  { email :: Text
  } deriving (Eq, Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''Account

instance FromHttpApiData [Account] where
  parseUrlPiece = parseUrlPieces . (splitOn ",")

instance ToHttpApiData [Account] where
  toUrlPiece = (intercalate ",") . toUrlPieces


newtype DateTime = DateTime
  { dateTime :: UTCTime
  } deriving (Eq, Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''DateTime


newtype ZonedDateTime = ZonedDateTime
  { dateTime :: Maybe ZonedTime
  } deriving (Generic, Show, Typeable, FromHttpApiData, ToHttpApiData)

deriveJSON defaultOptions ''ZonedDateTime

instance Eq ZonedDateTime where
  (==) =
    (\x y ->
      let
        toUTC :: ZonedDateTime -> Maybe UTCTime
        toUTC = (fmap zonedTimeToUTC) . (dateTime :: ZonedDateTime -> Maybe ZonedTime)
      in
        (toUTC x) == (toUTC y)
    )


data CalendarEvent = CalendarEvent
  { status :: Text
  , creator :: Account
  , attendees :: Maybe [Account]
  , summary :: Maybe Text
  , description :: Maybe Text
  , start :: Maybe ZonedDateTime
  , end :: Maybe ZonedDateTime
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''CalendarEvent

instance FromForm CalendarEvent

instance ToForm CalendarEvent


data CalendarEventList = CalendarEventList
  { kind :: Text
  , summary :: Text
  , items :: [CalendarEvent]
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''CalendarEventList


data GmailSend = GmailSend
  { id :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''GmailSend

instance FromForm GmailSend

instance ToForm GmailSend


data FileResource = FileResource
  { kind :: Text
  , id :: Text
  , name :: Text
  , mimeType :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''FileResource


data FileList = FileList
  { kind :: Text
  , files :: [FileResource]
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''FileList


data File = File
  { body :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''File


data Arbitrary
  deriving Typeable

instance Accept Arbitrary where
  contentTypes _ =
    ( "text" // "plain" ) :|
    [ "text" // "html"
    , "application" // "pdf"
    -- , other content type
    ]

instance MimeUnrender Arbitrary BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict
