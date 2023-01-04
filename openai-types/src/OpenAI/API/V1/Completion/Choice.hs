{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Completion.Choice where

import Data.Text (Text, pack)
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

instance ToJSON Choice where
  toJSON = genericToJSON options { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }
    where options = defaultOptions { omitNothingFields = True }
  toEncoding Choice {..} = pairs $ mconcat
    [ fromString "text" .= text
    , fromString "index" .= index
    , maybe mempty (\ x -> fromString "logprobs" .= x) logprobs
    , fromString "finish_reason" .= finishReason
    ]


instance FromJSON Choice where
  parseJSON (Object o) = do
    text <- o .: fromString "text"
    index <- o .: fromString "index"
    logprobs <- o .:? fromString "logprobs"
    finishReason <- o .: fromString "finish_reason"
    return $ Choice {..}
  parseJSON invalid = typeMismatch "Choice" invalid