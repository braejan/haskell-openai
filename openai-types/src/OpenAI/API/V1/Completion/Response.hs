{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.Response where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON (toEncoding), genericToJSON, pairs, KeyValue ((.=)), Value (Object), (.:))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch)
import OpenAI.API.V1.Completion.Choice ( Choice )
import OpenAI.API.V1.Common.Usage ( Usage, createUsage )
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

instance ToJSON Response
instance FromJSON Response

createResponse :: Response
createResponse = Response "" "" 0 "" [] createUsage