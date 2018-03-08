{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Google.Response

Define data types to represent all of the responses that are received from the Google API.
-}
module Google.Response where

import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH (Options(..), defaultOptions, deriveJSON)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (FromForm, ToForm)

data Token = Token
  { accessToken :: Text
  , tokenType :: Text
  , expiresIn :: Int
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON (defaultOptions {fieldLabelModifier = snakeCase}) ''Token

instance FromForm Token

instance ToForm Token

data CalendarEvent = CalendarEvent
  { status :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''CalendarEvent

instance FromForm CalendarEvent

instance ToForm CalendarEvent


data GmailSend = GmailSend
  { id :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''GmailSend

instance FromForm GmailSend

instance ToForm GmailSend
