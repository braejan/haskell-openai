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

-- instance ToJSON Choice where
--   toJSON = genericToJSON options { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }
--     where options = defaultOptions { omitNothingFields = True }
--   toEncoding Choice {..} = pairs $ mconcat
--     [ fromString "text" .= text
--     , fromString "index" .= index
--     , maybe mempty (\ x -> fromString "logprobs" .= x) logprobs
--     , fromString "finish_reason" .= finishReason
--     ]

instance ToJSON Request where
  toJSON = genericToJSON options { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_') }
    where options = defaultOptions { omitNothingFields = True }
  toEncoding Request {..} = pairs $ mconcat
    [ fromString "model" .= model
    , maybe mempty parseFromEitherTextOrArrayText prompt "prompt"
    , maybe mempty (\x -> fromString "suffix" .= x) suffix
    , maybe mempty (\x -> fromString "max_tokens" .= x) maxTokens
    , maybe mempty (\x -> fromString "temperature" .= x) temperature
    , maybe mempty (\x -> fromString "top_p" .= x) topP
    , maybe mempty (\x -> fromString "n" .= x) n
    , maybe mempty (\x -> fromString "stream" .= x) stream
    , maybe mempty (\x -> fromString "logprobs" .= x) logprobs
    , maybe mempty (\x -> fromString "echo" .= x) echo
    , maybe mempty parseFromEitherTextOrArrayText stop "stop"
    , maybe mempty (\x -> fromString "presence_penalty" .= x) presencePenalty
    , maybe mempty (\x -> fromString "frequency_penalty" .= x) frequencyPenalty
    , maybe mempty (\x -> fromString "best_of" .= x) bestOf
    , maybe mempty (\x -> fromString "logit_bias" .= x) logitBias
    , maybe mempty (\x -> fromString "user" .= x) user
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


parseEitherTextOrArrayText :: Value -> String -> Parser (Either Text [Text])
parseEitherTextOrArrayText (Array arr) _ = do
  list <- parseJSON (Array arr)
  return $ Right list
parseEitherTextOrArrayText (String s) _ = return $ Left s
parseEitherTextOrArrayText invalid key = typeMismatch key invalid