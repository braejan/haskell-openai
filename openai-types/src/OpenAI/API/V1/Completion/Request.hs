{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Completion.Request where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), toJSON, object, KeyValue ((.=)), Value (Null, Object, Array), pairs, withObject, (.:), (.:?), Key, Encoding)
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

instance ToJSON Request
instance FromJSON Request where
  -- | Parse a 'Request' from a 'Value'
  parseJSON :: Value -> Parser Request
  parseJSON = withObject "Request" $ \o -> do
    model <- o .: "model"
    prompt <- o .:? "prompt" >>= maybeEmpty . parseEitherTextOrArrayText
    suffix <- o .:? "suffix"
    maxTokens <- o .:? "max_tokens"
    temperature <- o .:? "temperature"
    topP <- o .:? "top_p"
    n <- o .:? "n"
    stream <- o .:? "stream"
    logprobs <- o .:? "logprobs"
    echo <- o .:? "echo"
    stop <- o .:? "stop" >>= maybeEmpty . parseEitherTextOrArrayText
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




-- -- | If the value is an 'Array', parse the array as a list of 'Text' values
-- parseEitherTextOrArrayText (Array arr) _ = do
--   list <- parseJSON (Array arr)
--   return $ Right list
-- -- | If the value is a 'String', parse the string as a 'Text' value
-- parseEitherTextOrArrayText (String str) _ = do
--   text <- parseJSON (String str)
--   return $ Left text


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

maybeEmpty :: Parser (Either a b) -> Parser (Maybe (Either a b))
maybeEmpty p = do
  either <- p
  return $ case either of
    Left _ -> Nothing
    Right _ -> Just either