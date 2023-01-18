{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Image.ImageData where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), Value (Object), KeyValue ((.=)), Encoding, (.:?))
import Data.Text (Text)
import Data.Aeson.Encoding (pairs)
import OpenAI.API.V1.Common.Helper (maybeEmpty)
import Data.Aeson.Types
    ( FromJSON(parseJSON),
      Value(Object),
      Parser,
      typeMismatch,
      ToJSON(toEncoding),
      Encoding,
      pairs,
      (.:?) )

data ImageData = ImageData {
    url :: Maybe Text,
    b64Json :: Maybe Text
} deriving (Show, Eq, Generic)


imageData :: ImageData
imageData = ImageData {
    url = Nothing,
    b64Json = Nothing
}

instance ToJSON ImageData where
  toEncoding :: ImageData -> Encoding
  toEncoding ImageData {..} = pairs $ mconcat
    [ maybeEmpty "url" url
    , maybeEmpty "b64_json" b64Json
    ]


instance FromJSON ImageData where
  parseJSON :: Value -> Parser ImageData
  parseJSON (Object o) = do
    url <- o .:? "url"
    b64Json <- o .:? "b64_json"
    return $ ImageData {..}
  parseJSON invalid = typeMismatch "ImageData" invalid