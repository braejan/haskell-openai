{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Image.Variation.Request where
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Encoding, ToJSON (toEncoding), Value (Object), pairs, KeyValue ((.=)), (.:), (.:?))
import Data.Aeson.Types (ToJSON, Parser, FromJSON (parseJSON), typeMismatch)
import OpenAI.API.V1.Common.Helper ( maybeEmpty )
import Data.Text (Text)


data Request = Request {
    image :: FilePath,
    n :: Maybe Int,
    size :: Maybe Text,
    responseFormat :: Maybe Text,
    user :: Maybe Text
} deriving (Show, Eq, Generic)

imageVariationRequest :: Request
imageVariationRequest = Request{
    image = "",
    n = Nothing,
    size = Nothing,
    responseFormat = Nothing,
    user = Nothing
}
instance ToJSON Request where
    toEncoding :: Request -> Encoding
    toEncoding Request {..} = pairs $ mconcat
        [ "image" .=  image
        , maybeEmpty "n" n
        , maybeEmpty "size" size
        , maybeEmpty "response_format" responseFormat
        , maybeEmpty "user" user
        ]

instance FromJSON Request where
    parseJSON :: Value -> Parser Request
    parseJSON (Object o) = do
        image <- o .: "image"
        n <- o .:? "n"
        size <- o .:? "size"
        responseFormat <- o .:? "response_format"
        user <- o .:? "user"
        return $ Request {..}
    parseJSON invalid = typeMismatch "CreateImageEditRequest" invalid