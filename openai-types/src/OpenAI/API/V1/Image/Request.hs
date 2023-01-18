{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Image.Request where
import Data.Text (Text)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), Value (Object), pairs, KeyValue ((.=)), (.:), (.:?), Encoding)
import OpenAI.API.V1.Common.Helper (maybeEmpty)
import Data.Aeson.Types (typeMismatch, Parser)
import GHC.Generics (Generic)


data Request = Request {
    prompt :: Text,
    -- ^ Required A text description of the desired image(s). The maximum length is 1000 characters.
    n :: Maybe Int,
    -- ^ Optional Defaults to 1. The number of images to generate. Must be between 1 and 10.
    size :: Maybe Text,
    -- ^ Optional Defaults to 1024x1024. The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024.
    responseFormat :: Maybe Text,
    -- ^ Optional Defaults to url. The format in which the generated images are returned. Must be one of url or b64_json.
    user :: Maybe Text
    -- ^ Optional A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse.
} deriving (Show, Eq, Generic)

imageRequest :: Request 
imageRequest = Request{
    prompt = "",
    n = Nothing,
    size = Nothing,
    responseFormat = Nothing,
    user = Nothing
}

instance ToJSON Request where
  toEncoding :: Request -> Encoding
  toEncoding Request {..} = pairs $ mconcat
    [ "prompt" .= prompt
    , maybeEmpty "n" n
    , maybeEmpty "size" size
    , maybeEmpty "response_format" responseFormat
    , maybeEmpty "user" user
    ]

instance FromJSON Request where
  parseJSON :: Value -> Parser Request
  parseJSON (Object o) = do
    prompt <- o .: "prompt"
    n <- o .:? "n"    
    size <- o .:? "size"
    responseFormat <- o .:? "response_format"
    user <- o .:? "user"
    return $ Request {..}
  parseJSON invalid = typeMismatch "Request" invalid