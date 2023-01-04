{-# LANGUAGE DeriveGeneric #-}
module OpenAI.API.V1.Completion.Response where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON, genericToJSON)
import Data.Aeson.Types (ToJSON(toJSON))
import OpenAI.API.V1.Completion.Choice
import OpenAI.API.V1.Completion.Usage
-- | Data type representing a response from the OpenAI API
data Response = Response
  { id :: Text
    -- ^ The ID of the response
  , object :: Text
    -- ^ The type of the response object (e.g. "text_completion")
  , created :: Int
    -- ^ The timestamp of when the response was created
  , model :: Text
    -- ^ The ID of the model used for the response
  , choices :: [Choice]
    -- ^ A list of 'Choice' objects representing the generated text completions
  , usage :: Usage
    -- ^ An 'Usage' object representing the usage details for the response
  } deriving (Show, Eq, Generic)

instance FromJSON Response where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON Response where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }