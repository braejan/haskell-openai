{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.RequestTest where

import Data.String (fromString)
import Test.Tasty (TestTree, testGroup)
import OpenAI.API.V1.Completion.Request (Request (..), completionRequest)
import Test.Tasty.HUnit (testCase, assertEqual)
import Data.Aeson (decode)
import Data.Map (fromList)
import qualified Data.Map as HashMap

-- Test suite definition
allCompletionRequestTest :: TestTree
allCompletionRequestTest =
  testGroup "Test suite for Module openai-types: Request"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testRequiredFields --3
    , testUntilPrompt --4
    , testUntilSuffix --5
    , testUntilMaxTokens --6
    , testUntilTemperature --7
    , testUntilTopP --8
    , testUntilN --9
    , testUntilStream --10
    , testUntilLogprobs --11
    , testUntilEcho --12
    , testUntilStop --13
    , testUntilPresencePenalty --14
    , testUntilFrequencyPenalty --15
    , testUntilBestOf --16
    , testUntilLogitBias --17
    , testUntilUser --18
    , testWithExtraFields --19
    , testWithWrongValues --20
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
    assertEqual "Test 4: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 5:
testUntilSuffix :: TestTree
testUntilSuffix = testCase "Deserialization of a JSON with just the required fields until suffix" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\"}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text" }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 5: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 6:
testUntilMaxTokens :: TestTree
testUntilMaxTokens = testCase "Deserialization of a JSON with just the required fields until max_tokens" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 6: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 7:
testUntilTemperature :: TestTree
testUntilTemperature = testCase "Deserialization of a JSON with just the required fields until temperature" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 7: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 8:
testUntilTopP :: TestTree
testUntilTopP = testCase "Deserialization of a JSON with just the required fields until top_p" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 8: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 9:
testUntilN :: TestTree
testUntilN = testCase "Deserialization of a JSON with just the required fields until n" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 9: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 10:
testUntilStream :: TestTree
testUntilStream = testCase "Deserialization of a JSON with just the required fields until stream" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 10: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 11:
testUntilLogprobs :: TestTree
testUntilLogprobs = testCase "Deserialization of a JSON with just the required fields until logprobs" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 11: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 12:
testUntilEcho :: TestTree
testUntilEcho = testCase "Deserialization of a JSON with just the required fields until echo" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 12: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 13:
testUntilStop :: TestTree
testUntilStop = testCase "Deserialization of a JSON with just the required fields until stop" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"]}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."] }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 13: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 14:
testUntilPresencePenalty :: TestTree
testUntilPresencePenalty = testCase "Deserialization of a JSON with just the required fields until presence_penalty" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 14: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 15:
testUntilFrequencyPenalty :: TestTree
testUntilFrequencyPenalty = testCase "Deserialization of a JSON with just the required fields until frequency_penalty" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5, frequencyPenalty = Just 0.5 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 15: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 16:
testUntilBestOf :: TestTree
testUntilBestOf = testCase "Deserialization of a JSON with just the required fields until best_of" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5,\"best_of\":3}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5, frequencyPenalty = Just 0.5, bestOf = Just 3 }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 16: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 17:
testUntilLogitBias :: TestTree
testUntilLogitBias = testCase "Deserialization of a JSON with just the required fields until logit_bias" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5,\"best_of\":3,\"logit_bias\":{\"This\":1.0}}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5, frequencyPenalty = Just 0.5, bestOf = Just 3, logitBias = Just $ HashMap.fromList [("This", 1.0)] }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 17: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 18:
testUntilUser :: TestTree
testUntilUser = testCase "Deserialization of a JSON with just the required fields until logit_bias" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5,\"best_of\":3,\"logit_bias\":{\"This\":1.0},\"user\":\"braejan\"}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5, frequencyPenalty = Just 0.5, bestOf = Just 3, logitBias = Just $ HashMap.fromList [("This", 1.0)], user = Just "braejan" }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 18: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 19:
testWithExtraFields :: TestTree
testWithExtraFields = testCase "Deserialization of a JSON with extra fields" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":0.8,\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5,\"best_of\":3,\"logit_bias\":{\"This\":1.0},\"user\":\"braejan\",\"extra_field\":\"extra_value\"}"
        expectedRequest = completionRequest { model = "model-id-1234", prompt = Just $ Left "This is a prompt", suffix = Just "suffix text", maxTokens = Just 16, temperature = Just 0.5, topP = Just 0.8, n = Just 3, stream = Just False, logprobs = Just 2, echo = Just True, stop = Just $ Right ["This is a test."], presencePenalty = Just 0.5, frequencyPenalty = Just 0.5, bestOf = Just 3, logitBias = Just $ HashMap.fromList [("This", 1.0)], user = Just "braejan" }
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 19: Deserialized value should be a valid Request" (Just expectedRequest) actualRequest

-- Test case 20:
testWithWrongValues :: TestTree
testWithWrongValues = testCase "Deserialization of a JSON with wrong field values" $ do
    let json = fromString "{\"model\":\"model-id-1234\",\"prompt\":\"This is a prompt\",\"suffix\":\"suffix text\",\"max_tokens\":16,\"temperature\":0.5,\"top_p\":\"0.8\",\"n\":3,\"stream\":false,\"logprobs\":2,\"echo\":true,\"stop\":[\"This is a test.\"],\"presence_penalty\":0.5,\"frequency_penalty\":0.5,\"best_of\":3,\"logit_bias\":{\"This\":1.0},\"user\":\"braejan\",\"extra_field\":\"extra_value\"}"
        expectedRequest = Nothing
        actualRequest = decode json :: Maybe Request
    assertEqual "Test 19: Deserialized value should be a Nothing" expectedRequest actualRequest
