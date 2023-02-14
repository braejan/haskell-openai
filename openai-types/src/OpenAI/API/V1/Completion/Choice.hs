{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.Choice where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), Options (fieldLabelModifier, omitNothingFields), FromJSON (parseJSON), Value (Object), genericToJSON, camelTo2, defaultOptions, pairs, KeyValue ((.=)), (.:), (.:?))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch)
import Data.Aeson.Key (fromString)
-- | Data type representing a choice object in an OpenAI response
data Choice = Choice
  { text :: Text
    -- ^ The generated text completion
  , index :: Int
    -- ^ The index of the choice
  , logprobs :: Maybe [Double]
    -- ^ A list of log probabilities for the generated tokens (if available)
  , finishReason :: Text
    -- ^ The reason the text generation was finished (e.g. "length")
  } deriving (Show, Eq, Generic)
-- | Options for encoding a Choice object to JSON
instance ToJSON Choice where
  toEncoding Choice {..} = pairs $ mconcat
    [ "text" .= text
    , "index" .= index
    , maybe mempty ("logprobs" .=) logprobs
    , "finish_reason" .= finishReason
    ]


instance FromJSON Choice where
  parseJSON (Object o) = do
    text <- o .: "text"
    index <- o .: "index"
    logprobs <- o .:? "logprobs"
    finishReason <- o .: "finish_reason"
    return $ Choice {..}
  parseJSON invalid = typeMismatch "Choice" invalid