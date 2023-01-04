{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Completion.Response where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), genericParseJSON, defaultOptions, camelTo2, ToJSON (toEncoding), genericToJSON, pairs, KeyValue ((.=)), Value (Object), (.:))
import Data.Aeson.Types (ToJSON(toJSON), typeMismatch)
import OpenAI.API.V1.Completion.Choice
import OpenAI.API.V1.Completion.Usage
import Data.Aeson.Key (fromString)
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

instance ToJSON Response where
  toEncoding Response {..} = pairs $ mconcat
    [ fromString "id" .= id
    , fromString "object" .= object
    , fromString "created" .= created
    , fromString "model" .= model
    , fromString "choices" .= choices
    , fromString "usage" .= usage
    ]

instance FromJSON Response where
  parseJSON (Object o) = do
    id <- o .: fromString "id"
    object <- o .: fromString "object"
    created <- o .: fromString "created"
    model <- o .: fromString "model"
    choices <- o .: fromString "choices"
    usage <- o .: fromString "usage"
    return $ Response{..}
  parseJSON invalid = typeMismatch "Response" invalid