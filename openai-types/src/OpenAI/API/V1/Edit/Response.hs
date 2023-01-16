{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.Response where
import Data.Text (Text)
import OpenAI.API.V1.Edit.Choice (Choice)
import OpenAI.API.V1.Common.Usage (Usage, createEmptyUsage)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), FromJSON, Value (Object), (.:), KeyValue ((.=)), pairs)
import Data.Aeson.Types (FromJSON(parseJSON), typeMismatch)


data Response = Response {
object :: Text,
created :: Int,
choices :: [Choice],
usage :: Usage
} deriving (Show, Eq, Generic)

createEmptyResponse :: Response
createEmptyResponse = Response {
    object = "",
    created = 0,
    choices = [],
    usage = createEmptyUsage
}

instance ToJSON Response where
  toEncoding Response {..} = pairs $ mconcat
    [ "object" .= object
    , "created" .= created
    , "choices" .= choices
    , "usage" .= usage
    ]

instance FromJSON Response where
  parseJSON (Object o) = do
    object <- o .: "object"
    created <- o .: "created"
    choices <- o .: "choices"
    usage <- o .: "usage"
    return $ Response{..}
  parseJSON invalid = typeMismatch "Response" invalid