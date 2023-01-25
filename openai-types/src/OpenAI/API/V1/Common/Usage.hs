{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Common.Usage where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier, omitNothingFields), genericParseJSON, defaultOptions, camelTo2, ToJSON (toEncoding), genericToJSON, pairs, KeyValue ((.=)), Value (Object), (.:), (.:?))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch)


-- | Data type representing usage details in an OpenAI response
data Usage = Usage
  { promptTokens :: Int
    -- ^ The number of tokens in the prompt
  , completionTokens :: Maybe Int
    -- ^ The number of tokens in the generated text completion
  , totalTokens :: Int
    -- ^ The total number of tokens used in the response
  } deriving (Show, Eq, Generic)

-- instance FromJSON Usage where
--   parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- instance ToJSON Usage where
--     toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Usage where
  toEncoding Usage{..} = pairs $ mconcat
    [ "prompt_tokens" .= promptTokens
    , "completion_tokens" .= completionTokens
    , "total_tokens" .= totalTokens
    ]

instance FromJSON Usage where
  parseJSON (Object o) = do
    promptTokens <- o .: "prompt_tokens"
    completionTokens <- o .:? "completion_tokens"
    totalTokens <- o .: "total_tokens"
    return $ Usage {..}
  parseJSON invalid = typeMismatch "Usage" invalid

createEmptyUsage :: Usage
createEmptyUsage = Usage {
  promptTokens = 0,
  completionTokens = Nothing,
  totalTokens = 0
}