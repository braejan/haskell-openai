{-# LANGUAGE DeriveGeneric #-}
module GPT3.Types.OpenAI.Request where

import Data.Text
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import GHC.Generics

data OpenAIRequest = OpenAIRequest
  { model :: Text
  , prompt :: Text
  , temperature :: Float
  , maxTokens :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON OpenAIRequest
toJSONByteString :: OpenAIRequest -> ByteString
toJSONByteString = encode

instance FromJSON OpenAIRequest

parseJSONFromByteString :: ByteString -> Maybe OpenAIRequest
parseJSONFromByteString = decode