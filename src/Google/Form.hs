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
  , Token(..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.Mail.Mime (Address(..), Mail(..), renderAddress, simpleMail)
import Web.FormUrlEncoded (Form(..), ToForm(toForm))
import Web.Internal.HttpApiData (toQueryParam)

import qualified Data.HashMap.Strict as HashMap

data Account = Account
  { email :: Text
  } deriving (Eq, Generic, Show, Typeable)

deriveJSON defaultOptions ''Account

data DateTime = DateTime
  { dateTime :: UTCTime
  } deriving (Eq, Generic, Show, Typeable)

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
