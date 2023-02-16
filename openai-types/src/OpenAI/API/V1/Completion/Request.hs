{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Completion.Request where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), toJSON, object, KeyValue ((.=)), Value (Null, Object, Array), pairs, withObject, (.:), (.:?), Key, Encoding, withText, withArray)
import Data.Aeson.Types
    ( pairs,
      (.:),
      (.:?),
      camelTo2,
      defaultOptions,
      genericToJSON,
      FromJSON(parseJSON),
      Options(omitNothingFields, fieldLabelModifier),
      Value(Object, String, Array),
      KeyValue((.=)),
      ToJSON(toJSON, toEncoding),
      typeMismatch,
      Parser )
import Data.Vector (toList)

-- | Data type representing a request to the OpenAI API
data Request = Request
  { model :: Text
  -- ^ Required. The ID of the model to use for the request.
  , prompt :: Maybe (Either Text [Text])
  -- ^ Optional. Defaults to The prompt(s) to generate completions for, encoded as a string, array of strings, array of tokens, or array of token arrays.
  , suffix :: Maybe Text
  -- ^ Optional. Defaults to null. The suffix that comes after a completion of inserted text.
  , maxTokens :: Maybe Int
  -- ^ Optional. Defaults to 16. The maximum number of tokens to generate in the completion.
  , temperature :: Maybe Float
  -- ^ Optional. Defaults to 1. What sampling temperature to use.
  , topP :: Maybe Float
  -- ^ Optional. Defaults to 1. An alternative to sampling with temperature, called nucleus sampling.
  , n :: Maybe Int
  -- ^ Optional. Defaults to 1. How many completions to generate for each prompt.
  , stream :: Maybe Bool
  -- ^ Optional. Defaults to false. Whether to stream back partial progress.
  , logprobs :: Maybe Int
  -- ^ Optional. Defaults to null. Include the log probabilities on the logprobs most likely tokens.
  , echo :: Maybe Bool
  -- ^ Optional. Defaults to false. Echo back the prompt in addition to the completion.
  , stop :: Maybe (Either Text [Text])
  -- ^ Optional. Defaults to null. Up to 4 sequences where the API will stop generating further tokens.
  , presencePenalty :: Maybe Float
  -- ^ Optional. Defaults to 0. Penalize new tokens based on whether they appear in the text so far.
  , frequencyPenalty :: Maybe Float
  -- ^ Optional. Defaults to 0. Penalize new tokens based on their existing frequency in the text so far.
  , bestOf :: Maybe Int
  -- ^ Optional. Defaults to 1. Generates best_of completions server-side and returns the "best" one.
  , logitBias :: Maybe (Map Text Float)
  -- ^ Optional. Defaults to null. Modify the likelihood of specified tokens appearing in the completion.
  , user :: Maybe Text
  -- ^ Optional.
  } deriving (Show, Eq, Generic)

-- Maybe exists a better way to do this

instance ToJSON Request where
  toEncoding Request {..} = pairs $ mconcat
    [ "model" .= model
    , maybe mempty parseFromEitherTextOrArrayText prompt "prompt"
    , maybeEmpty "suffix" suffix
    , maybeEmpty "max_tokens" maxTokens
    , maybeEmpty "temperature" temperature
    , maybeEmpty "top_p" topP
    , maybeEmpty "n" n
    , maybeEmpty "stream" stream
    , maybeEmpty "logprobs" logprobs
    , maybeEmpty "echo" echo
    , maybe mempty parseFromEitherTextOrArrayText stop "stop"
    , maybeEmpty "presence_penalty" presencePenalty
    , maybeEmpty "frequency_penalty" frequencyPenalty
    , maybeEmpty "best_of" bestOf
    , maybeEmpty "logit_bias" logitBias
    , maybeEmpty "user" user
    ]
instance FromJSON Request where
  -- | Parse a 'Request' from a 'Value'
  parseJSON :: Value -> Parser Request
  parseJSON = withObject "Request" $ \o -> do
    model <- o .: "model"
    prompt <- o .:? "prompt" >>= traverse parseEitherTextOrArrayText
    suffix <- o .:? "suffix"
    maxTokens <- o .:? "max_tokens"
    temperature <- o .:? "temperature"
    topP <- o .:? "top_p"
    n <- o .:? "n"
    stream <- o .:? "stream"
    logprobs <- o .:? "logprobs"
    echo <- o .:? "echo"
    stop <- o .:? "stop" >>= traverse parseEitherTextOrArrayText
    presencePenalty <- o .:? "presence_penalty"
    frequencyPenalty <- o .:? "frequency_penalty"
    bestOf <- o .:? "best_of"
    logitBias <- o .:? "logit_bias"
    user <- o .:? "user"
    return Request{..}

parseEitherTextOrArrayText :: Maybe Value -> Parser (Either Text [Text])
-- | If the value is 'Null', return an empty 'Left' value
parseEitherTextOrArrayText Nothing = return $ Left ""
-- | If the value is an 'Array', parse the array as a list of 'Text' values
parseEitherTextOrArrayText (Just (Array arr)) = do
  list <- parseJSON (Array arr)
  return $ Right list
-- | If the value is a 'String', parse the string as a 'Text' value
parseEitherTextOrArrayText (Just (String str)) = do
  text <- parseJSON (String str)
  return $ Left text


-- | Default 'Request' value
completionRequest :: Request
completionRequest = Request {
    model = "",
    prompt = Nothing,
    suffix = Nothing,
    maxTokens = Nothing,
    temperature = Nothing,
    topP = Nothing,
    n = Nothing,
    stream = Nothing,
    logprobs = Nothing,
    echo = Nothing,
    stop = Nothing,
    presencePenalty = Nothing,
    frequencyPenalty = Nothing,
    bestOf = Nothing,
    logitBias = Nothing,
    user = Nothing
}

parseFromEitherTextOrArrayText :: (KeyValue kv, ToJSON v1, ToJSON v2) => Either v1 v2 -> Key -> kv
parseFromEitherTextOrArrayText (Left x) key = key .= x
parseFromEitherTextOrArrayText (Right xs) key = key .= xs

maybeEmpty :: (Monoid b, KeyValue b, ToJSON v) => Key -> Maybe v -> b
maybeEmpty key = maybe mempty (key .=)