{-# LANGUAGE DeriveGeneric #-}
module OpenAI.Types.Usage where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON(toJSON))

-- | Data type representing usage details in an OpenAI response
data Usage = Usage
  { promptTokens :: Int
    -- ^ The number of tokens in the prompt
  , completionTokens :: Int
    -- ^ The number of tokens in the generated text completion
  , totalTokens :: Int
    -- ^ The total number of tokens used in the response
  } deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Usage where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }