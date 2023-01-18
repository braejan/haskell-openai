{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
module OpenAI.API.V1.Model.Response where
import GHC.Generics (Generic)
import OpenAI.API.V1.Model.Model (Model)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:), ToJSON (toEncoding), Encoding, pairs, KeyValue ((.=)))
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Text (Text)


data Response = Response {
  models :: [Model],
  object :: Text
} deriving (Eq, Show, Generic)

instance FromJSON Response where
    parseJSON :: Value -> Parser Response
    parseJSON (Object o) = do
        models <- o .: "data"
        object <- o .: "object"
        return $ Response {..}
    parseJSON invalid = typeMismatch "Response" invalid

instance ToJSON Response where
  toEncoding :: Response -> Encoding
  toEncoding Response {..} = pairs $ mconcat
    [ "data" .= models,
      "object" .= object
    ]