{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.RequestTest where

import Data.String (fromString)
import Test.Tasty (TestTree, testGroup)
import OpenAI.API.V1.Completion.Request (Request (..), completionRequest)
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Aeson (decode)
import Data.Map (fromList)

-- jsonString :: String
-- jsonString = "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":1,\"echo\":true,\"stop\":[\"This is the first stop sequence\",\"This is the second stop sequence\"],\"presence_penalty\":0.1,\"frequency_penalty\":0.2,\"best_of\":2,\"logit_bias\":{\"token1\":0.3,\"token2\":0.4},\"user\":\"user-id-1234\"}"

-- jsonStringWithArray :: String
-- jsonStringWithArray = "{\"model\":\"model-id-1234\",\"prompt\":[\"This is a prompt\",\"This is another prompt\"],\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":1,\"echo\":true,\"stop\":[\"This is the first stop sequence\",\"This is the second stop sequence\"],\"presence_penalty\":0.1,\"frequency_penalty\":0.2,\"best_of\":2,\"logit_bias\":{\"token1\":0.3,\"token2\":0.4},\"user\":\"user-id-1234\"}"

-- jsonStringMandatory :: String
-- jsonStringMandatory = "{\"model\":\"model-id-1234\"}"

-- Test suite definition
allCompletionRequestTest :: TestTree
allCompletionRequestTest =
  testGroup "Test suite for Module openai-types: Request"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testRequiredFields --3
    , testUntilPrompt --4
    -- , testAllFields --5
    -- , testExtraFields --6
    -- , testInvalidTextType --7
    -- , testInvalidIndexType --8
    ]

-- Test case 1:
testEmptyDeSerialization :: TestTree
testEmptyDeSerialization = testCase "Deserialization of an empty JSON and should be Nothing." $ do
  let request' = decode (fromString "{}") :: Maybe Request
  assertEqual "Test 1: Deserialized value should match default Request value" Nothing request'

-- Test case 2:
testEmptyString :: TestTree
testEmptyString = testCase "Deserialization of an empty String and should be Nothing." $ do
  let request' = decode (fromString "") :: Maybe Request
  assertEqual "Test 2: Deserialized value should be a Nothing value" Nothing request'

-- Test case 3:
testRequiredFields :: TestTree
testRequiredFields =  testCase "Deserialization of a JSON with just the required fields." $ do
    let json = fromString "{\"model\":\"model-id-1234\"}"
        expectedRequest = completionRequest { model = "model-id-1234" }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 3: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 4:
testUntilPrompt :: TestTree
testUntilPrompt = testCase "Deserialization of a JSON with just the required fields until prompt" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\"}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt" }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 3: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest