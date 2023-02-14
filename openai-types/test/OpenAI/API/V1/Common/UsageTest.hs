-- Description: Test suite for Module openai-types: Usage
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OpenAI.API.V1.Common.UsageTest where
import Test.Tasty (TestTree, testGroup)
import qualified Data.ByteString as BS
import Test.Tasty.HUnit (testCase, assertEqual)
import OpenAI.API.V1.Common.Usage (Usage (..), createUsage)
import Data.Aeson (decode)
import Data.String (IsString(fromString))

-- Test suite definition
allUsageTest :: TestTree
allUsageTest =
  testGroup "Test suite for Module openai-types: Usage"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testRequiredFields --3
    , testAllFields --4
    , testExtraFields --5
    , testInvalidPromptTokensType --6
    , testInvalidCompletionTokensType --7
    , testInvalidTotalTokensType --8
    , testNegativePromptTokensValue --9
    , testNegativeCompletionTokensValue --10
    , testNegativeTotalTokensValue --11
    , testValidAllFields --12
    , testValidAllFieldsWithExtraFields --13
    ]

-- Test case 1:
testEmptyDeSerialization :: TestTree
testEmptyDeSerialization = testCase "Deserialization of an empty JSON and should be Nothing." $ do
  let usage' = decode (fromString "{}") :: Maybe Usage
  assertEqual "Test 1: Deserialized value should match default Usage value" Nothing usage'

-- Test case 2:
testEmptyString :: TestTree
testEmptyString = testCase "Deserialization of an empty String and should be Nothing." $ do
  let usage' = decode (fromString "") :: Maybe Usage
  assertEqual "Test 2: Deserialized value should match default Usage value" Nothing usage'

-- Test case 3:
testRequiredFields :: TestTree
testRequiredFields = testCase "Deserialization of a JSON with just the required fields." $ do
  let json = fromString "{\"prompt_tokens\": 10, \"total_tokens\": 20}"
      expected = createUsage {promptTokens = 10, completionTokens = Nothing, totalTokens = 20}
      usage' = decode json :: Maybe Usage
  assertEqual "Test 3: Deserialized value should match expected value" (Just expected) usage'

-- Test case 4:
testAllFields :: TestTree
testAllFields = testCase "Deserialization of a JSON with all fields." $ do
  let json = fromString "{\"prompt_tokens\": 5, \"completion_tokens\": 10, \"total_tokens\": 20}"
      expected = createUsage {promptTokens = 5, completionTokens = Just 10, totalTokens = 20}
      usage' = decode json :: Maybe Usage
  assertEqual "Test 4: Deserialized value should match expected value" (Just expected) usage'

-- Test case 5:
testExtraFields :: TestTree
testExtraFields = testCase "Deserialization of a JSON with extra fields." $ do
  let json = fromString "{\"prompt_tokens\": 5, \"total_tokens\": 20, \"extra_field\": \"value\"}"
      expected = createUsage {promptTokens = 5, completionTokens = Nothing, totalTokens = 20}
      usage' = decode json :: Maybe Usage
  assertEqual "Test 5: Deserialized value should match expected value" (Just expected) usage'

-- Test case 6:
testInvalidPromptTokensType :: TestTree
testInvalidPromptTokensType = testCase "Deserialization of a JSON with invalid promptTokens value." $ do
  let usage' = decode (fromString "{\"prompt_tokens\": \"invalid\"}") :: Maybe Usage
  assertEqual "Test 6: Deserialization should fail and return Nothing" Nothing usage'

-- Test case 7:
testInvalidCompletionTokensType :: TestTree
testInvalidCompletionTokensType = testCase "Deserialization of a JSON with invalid completionTokens value." $ do
  let usage' = decode (fromString "{\"completion_tokens\": \"invalid\"}") :: Maybe Usage
  assertEqual "Test 7: Deserialization should fail and return Nothing" Nothing usage'

-- Test case 8:
testInvalidTotalTokensType :: TestTree
testInvalidTotalTokensType = testCase "Deserialization of a JSON with invalid totalTokens value." $ do
  let usage' = decode (fromString "{\"total_tokens\": \"invalid\"}") :: Maybe Usage
  assertEqual "Test 8: Deserialization should fail and return Nothing" Nothing usage'

-- Test case 9:
testNegativePromptTokensValue :: TestTree
testNegativePromptTokensValue = testCase "Deserialization of a JSON with negative value for promptTokens." $ do
  let json = "{\"prompt_tokens\": -1, \"total_tokens\": 20}"
      expected = Just $ Usage { promptTokens = -1, completionTokens = Nothing, totalTokens = 20 }
      usage' = decode (fromString json) :: Maybe Usage
  assertEqual "Test 9: Deserialized value should be Nothing" expected usage'

-- Test case 10:
testNegativeCompletionTokensValue :: TestTree
testNegativeCompletionTokensValue = testCase "Deserialization of a JSON with a negative value for completionTokens and should be Nothing." $ do
  let json = "{\"prompt_tokens\": 10, \"completion_tokens\": -5, \"total_tokens\": 15}"
      usage' = decode (fromString json) :: Maybe Usage
      expected = Just $ Usage { promptTokens = 10, completionTokens = Just (-5), totalTokens = 15 }
  assertEqual "Test 10: Deserialized value should match expected value" expected usage'

-- Test case 11:
testNegativeTotalTokensValue :: TestTree
testNegativeTotalTokensValue = testCase "Deserialization of a JSON with a negative value for totalTokens field and should be Nothing." $ do
  let usage' = decode "{\"prompt_tokens\": 1, \"completion_tokens\": 2, \"total_tokens\": -10}" :: Maybe Usage
      expected = Just $ Usage { promptTokens = 1, completionTokens = Just 2, totalTokens = -10 }
  assertEqual "Test 11: Deserialized value should match expected value" expected usage'

-- Test case 12:
testValidAllFields :: TestTree
testValidAllFields = testCase "Deserialization of a JSON with all fields." $ do
  let usage' = decode (fromString "{\"prompt_tokens\": 10, \"completion_tokens\": 20, \"total_tokens\": 30}") :: Maybe Usage
      expected = Just $ Usage { promptTokens = 10, completionTokens = Just 20, totalTokens = 30 }
  assertEqual "Test 12: Deserialized value should match expected value" expected usage'

-- Test case 13:
testValidAllFieldsWithExtraFields :: TestTree
testValidAllFieldsWithExtraFields = testCase "Deserialization of a JSON with all fields and additional fields" $ do
  let usageJson = "{\"prompt_tokens\": 1, \"completion_tokens\": 10, \"total_tokens\": 100, \"extra_field_1\": 0, \"extra_field_2\": \"additional field\"}"
      expected = Just $ Usage { promptTokens = 1, completionTokens = Just 10, totalTokens = 100 }
      usage' = decode (fromString usageJson) :: Maybe Usage
  assertEqual "Test 13: Deserialized value should match expected Usage value" expected usage'