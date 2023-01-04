{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Completion.Usage where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier, omitNothingFields), genericParseJSON, defaultOptions, camelTo2, ToJSON (toEncoding), genericToJSON, pairs, KeyValue ((.=)), Value (Object), (.:))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch)
import Data.Aeson.Key

-- | Data type representing usage details in an OpenAI response
data Usage = Usage
  { promptTokens :: Int
    -- ^ The number of tokens in the prompt
  , completionTokens :: Int
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
    [ fromString "prompt_tokens" .= promptTokens
    , fromString "completion_tokens" .= completionTokens
    , fromString "total_tokens" .= totalTokens
    ]

instance FromJSON Usage where
  parseJSON (Object o) = do
    promptTokens <- o .: fromString "prompt_tokens"
    completionTokens <- o .: fromString "completion_tokens"
    totalTokens <- o .: fromString "total_tokens"
    return $ Usage {..}
  parseJSON invalid = typeMismatch "Usage" invalid