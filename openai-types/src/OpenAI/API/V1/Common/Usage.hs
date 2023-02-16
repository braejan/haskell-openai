{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Common.Usage where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier, omitNothingFields), genericParseJSON, defaultOptions, camelTo2, ToJSON (toEncoding), genericToJSON, pairs, KeyValue ((.=)), Value (Object), (.:), (.:?))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch, Parser)

-- | Data type representing usage details in an OpenAI response
data Usage = Usage
  { promptTokens :: Int
    -- ^ The number of tokens in the prompt
  , completionTokens :: Maybe Int
    -- ^ The number of tokens in the generated text completion
  , totalTokens :: Int
    -- ^ The total number of tokens used in the response
  } deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON :: Value -> Parser Usage
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Usage where
  toJSON :: Usage -> Value
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

createUsage :: Usage 
createUsage = Usage 0 Nothing 0
