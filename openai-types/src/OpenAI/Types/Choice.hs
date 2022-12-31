{-# LANGUAGE DeriveGeneric #-}
module OpenAI.Types.Choice where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON(toJSON))

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

instance FromJSON Choice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Choice where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }