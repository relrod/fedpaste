{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Modernpaste.Types where

import Control.Applicative
import Data.Aeson
import qualified Data.Text as T
import Data.Time (UTCTime)

newtype Modernpaste =
  Modernpaste { modernpasteUrl :: String }
  deriving (Eq, Show)

data ModernpasteResponse a =
    MPSuccess a
  | MPError ErrorResponse
  deriving (Eq, Functor, Foldable, Show, Traversable)

instance FromJSON a => FromJSON (ModernpasteResponse a) where
  parseJSON v =
    (MPError <$> parseJSON v) <|>
    (MPSuccess <$> parseJSON v)

data ErrorResponse =
  ErrorResponse { errFailure :: T.Text
                , errMessage :: T.Text
                } deriving (Eq, Show)

instance FromJSON ErrorResponse where
  parseJSON =
    withObject "ErrorResponse" $ \v -> ErrorResponse
        <$> v .: "failure"
        <*> v .: "message"

data CreatePasteRequest =
  CreatePasteRequest { createReqContents :: T.Text
                     , createReqExpiryTime :: Maybe UTCTime
                     , createReqTitle :: Maybe T.Text
                     , createReqLanguage :: Maybe T.Text
                     , createReqPassword :: Maybe T.Text
                     } deriving (Eq, Show)

instance ToJSON CreatePasteRequest where
    toJSON (CreatePasteRequest contents expiry title lang pass) =
        object [ "contents" .= contents
               , "expiry_time" .= expiry
               , "title" .= title
               , "language" .= lang
               , "password" .= pass
               ]

data PasteIdRepr = PasteIdDecrypted Integer
                 | PasteIdEncrypted T.Text
                 deriving (Eq, Show)

instance FromJSON PasteIdRepr where
  parseJSON v =
      (PasteIdDecrypted <$> parseJSON v) <|>
      (PasteIdEncrypted <$> parseJSON v)

data Attachment =
  Attachment { attachmentFileName :: T.Text
             , attachmentFileSize :: Integer
             , attachmentMimeType :: T.Text
             } deriving (Eq, Show)

instance FromJSON Attachment where
  parseJSON =
    withObject "Attachment" $ \v -> Attachment
        <$> v .: "file_name"
        <*> v .: "file_size"
        <*> v .: "mime_type"

data CreatePasteResponse =
  CreatePasteResponse { createRespPostTime :: Integer
                      , createRespExpiryTime :: Maybe Integer
                      , createRespLanguage :: T.Text
                      , createRespTitle :: T.Text
                      , createRespViews :: Integer
                      , createRespIsActive :: Bool
                      , createRespIsPasswordProtected :: Bool
                      , createRespPasteIdRepr :: PasteIdRepr
                      , createRespContents :: T.Text
                      , createRespAttachments :: [Attachment]
                      --, createRespDeactivationToken :: Maybe T.Text -- ^ New
                      , createRespUrl :: T.Text
                      } deriving (Eq, Show)

instance FromJSON CreatePasteResponse where
  parseJSON =
    withObject "CreatePasteResponse" $ \v -> CreatePasteResponse
        <$> v .: "post_time"
        <*> v .: "expiry_time"
        <*> v .: "language"
        <*> v .: "title"
        <*> v .: "views"
        <*> v .: "is_active"
        <*> v .: "is_password_protected"
        <*> v .: "paste_id_repr"
        <*> v .: "contents"
        <*> v .: "attachments"
        -- <*> v .: "deactivation_token"
        <*> v .: "url"
