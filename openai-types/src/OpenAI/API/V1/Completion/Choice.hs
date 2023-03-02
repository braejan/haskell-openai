{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.Choice where
-- | Data type representing a choice object in an OpenAI completion response
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), Options (fieldLabelModifier, omitNothingFields), FromJSON (parseJSON), Value (Object), genericToJSON, camelTo2, defaultOptions, pairs, KeyValue ((.=)), (.:), (.:?), genericParseJSON)
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch, Parser)
import Data.Aeson.Key (fromString)
-- | Data type representing a choice object in an OpenAI response
data Choice = Choice
  { text :: Text
    -- ^ The generated text completion
  , index :: Int
    -- ^ The index of the choice
  , logprobs :: Maybe [Double]
    -- ^ A list of log probabilities for the generated tokens (if available)
  , finishReason :: Maybe Text
    -- ^ The reason the text generation was finished (e.g. "length")
  } deriving (Show, Eq, Generic)

instance FromJSON Choice where
  parseJSON :: Value -> Parser Choice
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Choice where
  toJSON :: Choice -> Value
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

createCompletionChoice :: Choice
createCompletionChoice = Choice "" 0 Nothing Nothing