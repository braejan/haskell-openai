{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Edit.Request where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, pairs, KeyValue ((.=)), FromJSON (parseJSON), Value (Object), (.:), (.:?))
import Data.Aeson.Types (ToJSON(toEncoding), typeMismatch)
import OpenAI.API.V1.Common.Helper (maybeEmpty)


data Request = Request { 
    model :: Text,
    -- ^ Required. ID of the model to use. You can use the List models API to see all of your available models,
    --             or see our Model overview for descriptions of them.
    input :: Maybe Text,
    -- ^ Optional. The input text to use as a starting point for the edit.
    instruction :: Text,
    -- ^ Required. The instruction that tells the model how to edit the prompt.
    n :: Maybe Int,
    -- ^ Optional. How many edits to generate for the input and instruction.
    temperature :: Maybe Double,
    -- ^ Optional. What sampling temperature to use. Higher values means the model will take more risks. Try 0.9
    --             for more creative applications, and 0 (argmax sampling) for ones with a well-defined answer. 
    --             We generally recommend altering this or top_p but not both.
    topP :: Maybe Double
    -- ^ Optional. An alternative to sampling with temperature, called nucleus sampling, where the model considers
    --             the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising
    --             the top 10% probability mass are considered. We generally recommend altering this or temperature
    --             but not both.
} deriving (Show, Eq, Generic)


createEmptyRequest :: Request
createEmptyRequest = Request {
    model = "",
    input = Nothing,
    instruction = "",
    n = Nothing,
    temperature = Nothing,
    topP = Nothing
}

instance ToJSON Request where
  toEncoding Request {..} = pairs $ mconcat
    [ "model" .= model
    , maybeEmpty "input" input
    , "instruction" .= instruction
    , maybeEmpty "n" n
    , maybeEmpty "temperature" temperature
    , maybeEmpty "top_p" topP
    ]

instance FromJSON Request where
  parseJSON (Object o) = do
    model <- o .: "model"
    input <- o .:? "input"
    instruction <- o .: "instruction"
    n <- o .:? "n"
    temperature <- o .:? "temperature"
    topP <- o .:? "top_p"
    return $ Request {..}
  parseJSON invalid = typeMismatch "Request" invalid