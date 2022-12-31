{-# LANGUAGE DeriveGeneric #-}
module OpenAI.Types.Request where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON(toJSON))

-- | Data type representing a request to the OpenAI API
data OpenAIRequest = OpenAIRequest
  { model :: Text
    -- ^ The ID of the model to use for the request
  , prompt :: Text
    -- ^ The prompt to send to the model
  , temperature :: Float
    -- ^ The temperature parameter to use for the request
  , maxTokens :: Int
    -- ^ The maximum number of tokens to generate in the response
  } deriving (Show, Eq, Generic)

instance FromJSON OpenAIRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON OpenAIRequest where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }