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

jsonString :: String
jsonString =
  "{ \
  \  \"model\":\"text-davinci-003\", \
  \  \"prompt\":\"Say this is a test\", \
  \  \"max_tokens\":7, \
  \  \"temperature\":0, \
  \  \"top_p\":1, \
  \  \"n\":1, \
  \  \"stream\":false, \
  \  \"logprobs\":null, \
  \  \"stop\":\"\\n\" \
  \}"

-- Test suite definition
allRequestTest :: TestTree
allRequestTest =
  testGroup "Test suite for Module openai-types: Request"
    [ testSerializationAnDeserialization,
      testStringSerialization,
      testEmptyStringSerialization,
      testDeSerialization
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
      expected = Right createDefaultRequest
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
  let request = createDefaultRequest
  let actual = encode request
  assertEqual "4=>Parsed value should match expected value" expected actual


-- | Create a new 'Request' value with default test values
createDefaultRequest :: Request
createDefaultRequest = Request
  { model = pack "text-davinci-003"
  , prompt = Just $ Left (pack "Say this is a test")
  , suffix = Nothing
  , maxTokens = Just 7
  , temperature = Just 0
  , topP = Just 1
  , n = Just 1
  , stream = Just False
  , logprobs = Nothing
  , echo = Nothing
  , stop = Just $ Left (pack "\n")
  , presencePenalty = Nothing
  , frequencyPenalty = Nothing
  , bestOf = Nothing
  , logitBias = Nothing
  , user = Nothing
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