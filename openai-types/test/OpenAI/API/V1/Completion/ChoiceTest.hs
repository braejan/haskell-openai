module OpenAI.API.V1.Completion.ChoiceTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Completion.Choice(Choice(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)


jsonString :: String
jsonString = "{\"text\":\"This is indeed a test\",\"index\":0,\"finish_reason\":\"length\"}"

jsonStringAllFields :: String
jsonStringAllFields = "{\"text\":\"This is indeed a test\",\"index\":0,\"logprobs\":[0.0,1.0,2.0],\"finish_reason\":\"length\"}"

-- Test suite definition
allChoiceTest :: TestTree
allChoiceTest =
  testGroup "Test suite for Module openai-types: Choice"
    [ testSerializationAnDeserialization,
      testStringSerialization,
      testEmptyStringSerialization,
      testDeSerialization,
      testDeSerializationLogprobs
    ]

-- Test case 1:
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a Choice JSON" $ do
    let expected = createDefaultChoice
        json = encode expected
        choice' = decode json
    assertEqual "1=>Deserialized value should match original value" (Just expected) choice'

-- Test case 2:
testStringSerialization :: TestTree
testStringSerialization = testCase "Serialization of String value" $ do
  let json = BS.pack jsonString
      expected = Right createDefaultChoice
      actual = eitherDecode json :: Either String Choice
  assertEqual "2=>Parsed value should match expected value" expected actual

-- Test case 3:
testEmptyStringSerialization :: TestTree
testEmptyStringSerialization = testCase "Serialization of a Empty string value" $ do
  let json = BS.pack ""
      actual = eitherDecode json :: Either String Choice
  assertBool "3=>Parsed value should match expected value" (isLeft actual)


-- Test case 4:
testDeSerialization :: TestTree
testDeSerialization = testCase "Deserialization of a default Choice test to String." $ do
  let expected = BS.pack jsonString
  let choice = createDefaultChoice
  let actual = encode choice
  assertEqual "4=>Parsed value should match expected value" expected actual

-- Test case 5:
testDeSerializationLogprobs :: TestTree
testDeSerializationLogprobs = testCase "Deserialization of a default Choice test to String." $ do
  let expected = BS.pack jsonStringAllFields
  let choice = createDefaultChoice {logprobs = Just [0.0,1.0,2.0]}
  let actual = encode choice
  assertEqual "5=>Parsed value should match expected value" expected actual

-- | Create a new 'Choice' value with default test values
createDefaultChoice = Choice
  {
  text = pack "This is indeed a test",
  index = 0,
  logprobs = Nothing,
  finishReason = pack "length"
  }

-- | Create a new 'Choice' value with default test values
createEmptyChoice :: Choice
createEmptyChoice = Choice
  { text = pack ""
  , index = 0
  , logprobs = Nothing
  , finishReason = pack ""
  }