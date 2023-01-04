{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Completion.Request where

import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), toJSON, object, KeyValue ((.=)), Value (Null, Object, Array), pairs, withObject, (.:), (.:?))
import Data.Aeson.Key(fromString)
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

instance ToJSON Request where
  toEncoding Request {..} = pairs $ mconcat
    [ fromString "model" .= model
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

parseFromEitherTextOrArrayText (Left x) key = fromString key .= x
parseFromEitherTextOrArrayText (Right xs) key = fromString key .= xs

instance FromJSON Request where
  parseJSON (Object o) = do
    model <- o .: fromString "model"
    prompt <- do
      promptObject <- o .: fromString "prompt"
      choice <- parseEitherTextOrArrayText promptObject "prompt"
      case choice of
        Left choiceText -> return $ Just $ Left choiceText
        Right choiceArray -> return $ Just $ Right choiceArray
    suffix <- o .:? fromString "suffix"
    maxTokens <- o .:? fromString "max_tokens"
    temperature <- o .:? fromString "temperature"
    topP <- o .:? fromString "top_p"
    n <- o .:? fromString "n"
    stream <- o .:? fromString "stream"
    logprobs <- o .:? fromString "logprobs"
    echo <- o .:? fromString "echo"
    stop <- do
      promptObject <- o .: fromString "stop"
      choice <- parseEitherTextOrArrayText promptObject "stop"
      case choice of
        Left choiceText -> return $ Just $ Left choiceText
        Right choiceArray -> return $ Just $ Right choiceArray
    presencePenalty <- o .:? fromString "presence_penalty"
    frequencyPenalty <- o .:? fromString "frequency_penalty"
    bestOf <- o .:? fromString "best_of"
    logitBias <- o .:? fromString "logit_bias"
    user <- o .:? fromString "user"
    return $ Request {..}
  parseJSON invalid = typeMismatch "Request" invalid


-- | Parse 'Either' 'Text' or 'Array' 'Text'
--
-- This function is used to parse fields that can be represented as a single string
-- or an array of strings, such as the 'prompt' and 'stop' fields in the 'Request' data type.
parseEitherTextOrArrayText :: Value -> String -> Parser (Either Text [Text])
-- | If the value is an 'Array', parse the array as a list of 'Text' values
parseEitherTextOrArrayText (Array arr) _ = do
  list <- parseJSON (Array arr)
  return $ Right list
-- | If the value is a 'String', return the 'String' as a 'Left' 'Text' value
parseEitherTextOrArrayText (String s) _ = return $ Left s
-- | If the value is not a 'String' or an 'Array', throw a 'typeMismatch' error
parseEitherTextOrArrayText invalid key = typeMismatch key invalid

-- | Create a 'KeyValue' pair from a 'Maybe' value, using 'mempty' if the value is 'Nothing'
--
-- This function is used to create 'KeyValue' pairs for optional fields in the 'Request' data type.
maybeEmpty :: (Monoid b, KeyValue b, ToJSON v) => String -> Maybe v -> b
maybeEmpty key = maybe mempty (\x -> fromString key .= x)
