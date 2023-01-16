{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.Choice where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, Value (Object), (.:), pairs, KeyValue ((.=)))
import Data.Aeson.Types (ToJSON(toEncoding), FromJSON (parseJSON), typeMismatch)


data Choice = Choice {
text :: Text,
index :: Int
} deriving (Show, Eq, Generic)

createEmptyChoice :: Choice 
createEmptyChoice = Choice {
    text = "",
    index = 0
}

instance ToJSON Choice where
  toEncoding Choice {..} = pairs $ mconcat
    [ "text" .= text
    , "index" .= index
    ]


instance FromJSON Choice where
  parseJSON (Object o) = do
    text <- o .: "text"
    index <- o .: "index"
    return $ Choice {..}
  parseJSON invalid = typeMismatch "Choice" invalid