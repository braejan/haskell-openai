{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Image.Edit.Request where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Aeson (ToJSON (toEncoding), pairs, KeyValue ((.=)), FromJSON (parseJSON), Value (Object), (.:), (.:?), Encoding)
import OpenAI.API.V1.Common.Helper (maybeEmpty)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as B64
import Data.Aeson.Types
    ( FromJSON(parseJSON),
      Value(Object),
      typeMismatch,
      ToJSON(toEncoding),
      KeyValue((.=)),
      pairs,
      (.:),
      (.:?), Parser )

data Request = Request {
    image :: String,
    mask :: Maybe String,
    prompt :: Text,
    n :: Maybe Int,
    size :: Maybe Text,
    responseFormat :: Maybe Text,
    user :: Maybe Text
} deriving (Show, Eq, Generic)

imageEditRequest :: Request
imageEditRequest = Request{
    image = "",
    mask = Nothing,
    prompt = "",
    n = Nothing,
    size = Nothing,
    responseFormat = Nothing,
    user = Nothing
}
instance ToJSON Request where
    toEncoding :: Request -> Encoding
    toEncoding Request {..} = pairs $ mconcat
        [ "image" .=  image
        , maybeEmpty "mask" mask
        , "prompt" .= prompt
        , maybeEmpty "n" n
        , maybeEmpty "size" size
        , maybeEmpty "response_format" responseFormat
        , maybeEmpty "user" user
        ]

instance FromJSON Request where
    parseJSON :: Value -> Parser Request
    parseJSON (Object o) = do
        image <- o .: "image"
        mask <- o .:? "mask"
        prompt <- o .: "prompt"
        n <- o .:? "n"
        size <- o .:? "size"
        responseFormat <- o .:? "response_format"
        user <- o .:? "user"
        return $ Request {..}
    parseJSON invalid = typeMismatch "CreateImageEditRequest" invalid
