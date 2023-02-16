{-#LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.ResponseTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Completion.Response(Response(..), createResponse)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import OpenAI.API.V1.Completion.Choice (Choice (..), createCompletionChoice)
import OpenAI.API.V1.Common.Usage (Usage (..), createUsage)
import Data.String (fromString)
import qualified OpenAI.API.V1.Completion.Response as CR


-- Test suite definition
allCompletionResponseTest :: TestTree
allCompletionResponseTest =
  testGroup "Test suite for Module openai-types: Response"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testAllRequiredFields --3
    , testAllFields --4
    , testExtraFields --5
    , testWithWrongFields --6
    , testAllFieldsWithChoices --7
    ]

-- Test case 1:
testEmptyDeSerialization :: TestTree
testEmptyDeSerialization = testCase "Deserialization of an empty JSON and should be Nothing." $ do
  let response' = decode (fromString "{}") :: Maybe Response
  assertEqual "Test 1: Deserialized value should match default Response value" Nothing response'

-- Test case 2:
testEmptyString :: TestTree
testEmptyString = testCase "Deserialization of an empty String and should be Nothing." $ do
  let response' = decode (fromString "") :: Maybe Response
  assertEqual "Test 2: Deserialized value should be a Nothing value" Nothing response'

-- Test case 3:
testAllRequiredFields :: TestTree
testAllRequiredFields = testCase "Deserialization of a JSON with all required fields." $ do
  let expectedResponse = Just createResponse
      actualResponse = decode (fromString "{\"id\":\"\",\"object\":\"\",\"created\":0,\"model\":\"\",\"choices\":[]}") :: Maybe Response
  assertEqual "Test 3: Deserialized value should match a valid Response value" expectedResponse actualResponse

-- Test case 4:
testAllFields :: TestTree
testAllFields = testCase "Deserialization of a JSON with all fields." $ do
  let usage = Just $ createUsage {promptTokens = 5, completionTokens = Just 10, totalTokens = 20}
      expectedResponse = Just $ createResponse {CR.id = "sample-id", CR.object = "sample object", created = 10, model = "davinci-03", choices = [], usage = usage}
      actualResponse = decode (fromString "{\"id\":\"sample-id\",\"object\":\"sample object\",\"created\":10,\"model\":\"davinci-03\",\"choices\":[],\"usage\":{\"prompt_tokens\": 5, \"completion_tokens\": 10, \"total_tokens\": 20}}") :: Maybe Response
  assertEqual "Test 4: Deserialized value should match a valid Response value" expectedResponse actualResponse

-- Test case 5:
testExtraFields :: TestTree
testExtraFields = testCase "Deserialization of a JSON with extra fields." $ do
  let usage = Just $ createUsage {promptTokens = 5, completionTokens = Just 10, totalTokens = 20}
      expectedResponse = Just $ createResponse {CR.id = "sample-id", CR.object = "sample object", created = 10, model = "davinci-03", choices = [], usage = usage}
      actualResponse = decode (fromString "{\"id\":\"sample-id\",\"object\":\"sample object\",\"created\":10,\"model\":\"davinci-03\",\"choices\":[],\"usage\":{\"prompt_tokens\": 5, \"completion_tokens\": 10, \"total_tokens\": 20},\"extra_field\":1234567890}") :: Maybe Response
  assertEqual "Test 4: Deserialized value should match a valid Response value" expectedResponse actualResponse

-- Test case 6:
testWithWrongFields :: TestTree
testWithWrongFields = testCase "Deserialization of a JSON with wrong fields." $ do
  let actualResponse = decode (fromString "{\"id\":1234567890,\"object\":1234567890,\"created\":\"10\",\"model\":1234567890,\"choices\":1234567890,\"usage\":1234567890}") :: Maybe Response
  assertBool "Test 6: Deserialized value should be a Nothing value" (isLeft (maybe (Left "Error") Right actualResponse))

-- Test case 7:
testAllFieldsWithChoices :: TestTree
testAllFieldsWithChoices = testCase "Deserialization of a JSON with all fields and choice." $ do
  let usage = Just $ createUsage {promptTokens = 5, completionTokens = Just 10, totalTokens = 20}
      choices = [createCompletionChoice {text = "Which color do you prefer?", index = 0, logprobs = Just [0.1,0.2], finishReason = "length"}]
      expectedResponse = Just $ createResponse {CR.id = "sample-id", CR.object = "sample object", created = 10, model = "davinci-03", choices = choices, usage = usage}
      actualResponse = decode (fromString "{\"id\":\"sample-id\",\"object\":\"sample object\",\"created\":10,\"model\":\"davinci-03\",\"choices\":[{\"text\":\"Which color do you prefer?\",\"index\":0,\"logprobs\":[0.1,0.2],\"finish_reason\":\"length\"}],\"usage\":{\"prompt_tokens\": 5, \"completion_tokens\": 10, \"total_tokens\": 20}}") :: Maybe Response
  assertEqual "Test 4: Deserialized value should match a valid Response value" expectedResponse actualResponse

