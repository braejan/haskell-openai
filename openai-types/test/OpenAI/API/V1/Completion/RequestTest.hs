module OpenAI.API.V1.Completion.RequestTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Completion.Request(Request(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map

jsonString :: String
jsonString = "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":1,\"echo\":true,\"stop\":[\"This is the first stop sequence\",\"This is the second stop sequence\"],\"presence_penalty\":0.1,\"frequency_penalty\":0.2,\"best_of\":2,\"logit_bias\":{\"token1\":0.3,\"token2\":0.4},\"user\":\"user-id-1234\"}"

jsonStringWithArray :: String
jsonStringWithArray = "{\"model\":\"model-id-1234\",\"prompt\":[\"This is a prompt\",\"This is another prompt\"],\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":1,\"echo\":true,\"stop\":[\"This is the first stop sequence\",\"This is the second stop sequence\"],\"presence_penalty\":0.1,\"frequency_penalty\":0.2,\"best_of\":2,\"logit_bias\":{\"token1\":0.3,\"token2\":0.4},\"user\":\"user-id-1234\"}"

jsonStringMandatory :: String
jsonStringMandatory = "{\"model\":\"model-id-1234\"}"

-- Test suite definition
allRequestTest :: TestTree
allRequestTest =
  testGroup "Test suite for Module openai-types: Request"
    [ testSerializationAnDeserialization,
      testStringSerialization,
      testEmptyStringSerialization,
      testDeSerialization,
      testDeSerializationWithArray,
      testDeSerializationMandatory
    ]

-- Test case 1:
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a Request JSON" $ do
    let expected = createDefaultRequest
        json = encode expected
        request' = decode json
    assertEqual "1=>Deserialized value should match original value" (Just expected) request'

-- Test case 2:
testStringSerialization :: TestTree
testStringSerialization = testCase "Serialization of String value" $ do
  let json = BS.pack jsonString
      expected = Right createDefaultRequest {prompt = Just $ Left (pack "This is a prompt")}
      actual = eitherDecode json :: Either String Request
  assertEqual "2=>Parsed value should match expected value" expected actual

-- Test case 3:
testEmptyStringSerialization :: TestTree
testEmptyStringSerialization = testCase "Serialization of a Empty string value" $ do
  let json = BS.pack ""
      actual = eitherDecode json :: Either String Request
  assertBool "3=>Parsed value should match expected value" (isLeft actual)


-- Test case 4:
testDeSerialization :: TestTree
testDeSerialization = testCase "Deserialization of a default Request test to String." $ do
  let expected = BS.pack jsonString
  let request = createDefaultRequest {prompt = Just $ Left (pack "This is a prompt")}
  let actual = encode request
  assertEqual "4=>Parsed value should match expected value" expected actual

-- Test case 5:
testDeSerializationWithArray :: TestTree
testDeSerializationWithArray = testCase "Deserialization of a default Request test to String." $ do
  let expected = BS.pack jsonStringWithArray
  let request = createDefaultRequest
  let actual = encode request
  assertEqual "5=>Parsed value should match expected value" expected actual
-- Test case 6:
testDeSerializationMandatory :: TestTree
testDeSerializationMandatory = testCase "Deserialization of a default Request test to String." $ do
  let expected = BS.pack jsonStringMandatory
  let request = createEmptyRequest {model = pack "model-id-1234"}
  let actual = encode request
  assertEqual "6=>Parsed value should match expected value" expected actual


-- | Create a new 'Request' value with default test values
createDefaultRequest :: Request
createDefaultRequest = Request
  { model = pack "model-id-1234"
  , prompt = Just $ Right [pack "This is a prompt", pack  "This is another prompt"]
  , suffix = Just $ pack "suffix text"
  , maxTokens = Just 16
  , temperature = Just 0.5
  , topP = Just 0.8
  , n = Just 3
  , stream = Just False
  , logprobs = Just 1
  , echo = Just True
  , stop = Just $ Right [pack  "This is the first stop sequence", pack "This is the second stop sequence"]
  , presencePenalty = Just 0.1
  , frequencyPenalty = Just 0.2
  , bestOf = Just 2
  , logitBias = Just $ Map.fromList [(pack "token1", 0.3), (pack "token2", 0.4)]
  , user = Just $ pack "user-id-1234"
  }

-- | Create a new 'Request' value with default test values
createEmptyRequest :: Request
createEmptyRequest = Request
  { model = pack ""
  , prompt = Nothing
  , suffix = Nothing
  , maxTokens = Nothing
  , temperature = Nothing
  , topP = Nothing
  , n = Nothing
  , stream = Nothing
  , logprobs = Nothing
  , echo = Nothing
  , stop = Nothing
  , presencePenalty = Nothing
  , frequencyPenalty = Nothing
  , bestOf = Nothing
  , logitBias = Nothing
  , user = Nothing
  }