{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module OpenAI.API.V1.Completion.Request where

import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), toJSON, object, KeyValue ((.=)), Value (Null, Object, Array), pairs, withObject, (.:), (.:?))
import Data.Aeson.Key(fromString)
import Data.Aeson.Types
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
  , logitBias :: Maybe (HM.HashMap Text Float)
  -- ^ Optional. Defaults to null. Modify the likelihood of specified tokens appearing in the completion.
  , user :: Maybe Text
  -- ^ Optional.
  } deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON Request{..} =
    object
      [ fromString "model" .= model,
        fromString "prompt" .= case prompt of
          Just (Left x) -> object [fromString "Left" .= x]
          Just (Right xs) -> object [fromString "Right" .= xs]
          Nothing -> object [],
        fromString "suffix" .= suffix,
        fromString "maxTokens" .= maxTokens,
        fromString "temperature" .= temperature,
        fromString "topP" .= topP,
        fromString "n" .= n,
        fromString "stream" .= stream,
        fromString "logprobs" .= logprobs,
        fromString "echo" .= echo,
        fromString "stop" .= case stop of
          Just (Left x) -> object [fromString "Left" .= x]
          Just (Right xs) -> object [fromString "Right" .= xs]
          Nothing -> object [],
        fromString "presencePenalty" .= presencePenalty,
        fromString "frequencyPenalty" .= frequencyPenalty,
        fromString "bestOf" .= bestOf,
        fromString "logitBias" .= logitBias,
        fromString "user" .= user
      ]

instance FromJSON Request where
  parseJSON (Object o) = do
    model <- o .: fromString "model"
    prompt <- Just <$> parseEitherStringOrArray o (fromString "prompt")
    suffix <- o .:? fromString "suffix"
    maxTokens <- o .:? fromString "maxTokens"
    temperature <- o .:? fromString "temperature"
    topP <- o .:? fromString "topP"
    n <- o .:? fromString "n"
    stream <- o .:? fromString "stream"
    logprobs <- o .:? fromString "logprobs"
    echo <- o .:? fromString "echo"
    stop <- o .:? fromString "stop"
    presencePenalty <- o .:? fromString "presencePenalty"
    frequencyPenalty <- o .:? fromString "frequencyPenalty"
    bestOf <- o .:? fromString "bestOf"
    logitBias <- o .:? fromString "logitBias"
    user <- o .:? fromString "user"
    return Request{..}
  parseJSON invalid = typeMismatch "RequestMinified" invalid

-- parseEitherStringOrArray :: Object -> Key -> Parser (Either Text [Text])
-- parseEitherStringOrArray o field = do
--   mbValue <- o .:? field
--   case mbValue of
--     Just (String s) -> return $ Left s
--     Just (Array a) -> do
--       elements <- parseJSON (Array a)
--       return $ Right elements
--     Just _ -> fail $ "Error in $." ++ show field ++ ": expected a string or an array"
--     Nothing -> return $ Right []

parseEitherStringOrArray :: Object -> Key -> Parser (Either Text [Text])
parseEitherStringOrArray o field = do
  mbValue <- o .:? field
  case mbValue of
    Just (String s) -> return $ Left s
    Just (Array a) -> do
      elements <- parseJSON (Array a)
      return $ Right elements
    Just Null -> return $ Left (pack "")
    Just _ -> fail $ "Error in $." ++ show field ++ ": expected a string or an array"
    Nothing -> return $ Left (pack "")
