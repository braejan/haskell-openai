{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.ChoiceTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Completion.Choice(Choice(..), createCompletionChoice)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe, isNothing)
import Data.Either (isLeft)
import Data.String (fromString)

-- | Test serialization and deserialization of a Choice test suite
allCompletionChoiceTest :: TestTree
allCompletionChoiceTest =
  testGroup "Test suite for Module Completion openai-types: Choice"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testRequiredFields --3
    , testAllFields --4
    , testExtraFields --5
    , testInvalidTextType --6
    , testInvalidIndexType --7
    , testCreateCompletionChoice --8
    ]


-- Test case 1:
testEmptyDeSerialization :: TestTree
testEmptyDeSerialization = testCase "Deserialization of an empty JSON and should be Nothing." $ do
  let choice' = decode (fromString "{}") :: Maybe Choice
  assertEqual "Test 1: Deserialized value should match default Choice value" Nothing choice'

-- Test case 2:
testEmptyString :: TestTree
testEmptyString = testCase "Deserialization of an empty string to Choice should result in an error." $ do
  let choice' = decode $ fromString "\"\"" :: Maybe Choice
  assertBool "Test 2: Deserializing an empty string to Choice should result in an error." (isNothing choice')

-- Test case 3:
testRequiredFields :: TestTree
testRequiredFields = testCase "Deserialization of incomplete JSON to Choice should result in an error." $ do
  let input = "{\"text\":\"hello\"}" -- "index" field is missing
      choice' = decode (fromString input) :: Maybe Choice
  assertBool "Test 3: Deserializing incomplete JSON to Choice should result in an error." (isNothing choice')

-- Test case 4:
testAllFields :: TestTree
testAllFields = testCase "Deserialization of complete JSON to Choice should result in a valid Choice." $ do
  let input = "{\"text\":\"Which color do you prefer?\",\"index\":0,\"logprobs\":[0.1,0.2],\"finish_reason\":\"length\"}"
      expectedChoice = Choice "Which color do you prefer?" 0 (Just [0.1,0.2]) "length"
      actualChoice = decode (fromString input) :: Maybe Choice
  assertEqual "Test 4: Deserialized value should match expected Choice value" (Just expectedChoice) actualChoice

-- Test case 5:
testExtraFields :: TestTree
testExtraFields = testCase "Deserialization of JSON with extra fields to Choice should result in a valid Choice." $ do
  let input = "{\"text\":\"Which color do you prefer?\",\"index\":0,\"logprobs\":[0.1,0.2],\"finish_reason\":\"length\",\"extra_field\":\"extra_value\"}"
      expectedChoice = Choice "Which color do you prefer?" 0 (Just [0.1,0.2]) "length"
      actualChoice = decode (fromString input) :: Maybe Choice
  assertEqual "Test 5: Deserializing JSON with extra fields to Choice should result in a valid Choice." (Just expectedChoice) actualChoice

-- Test case 6:
testInvalidTextType :: TestTree
testInvalidTextType = testCase "Deserialization of JSON with invalid type for text field to Choice should result in an error." $ do
  let input = "{\"text\":0,\"index\":0,\"logprobs\":[0.1,0.2],\"finish_reason\":\"length\"}"
      choice' = decode (fromString input) :: Maybe Choice
  assertBool "Test 6: Deserializing JSON with invalid type for text field to Choice should result in an error." (isNothing choice')

-- Test case 7:
testInvalidIndexType :: TestTree
testInvalidIndexType = testCase "Deserialization of JSON with invalid type for index field to Choice should result in an error." $ do
  let input = "{\"text\":\"Which color do you prefer?\",\"index\":\"0\",\"logprobs\":[0.1,0.2],\"finish_reason\":\"length\"}"
      choice' = decode (fromString input) :: Maybe Choice
  assertBool "Test 7: Deserializing JSON with invalid type for index field to Choice should result in an error." (isNothing choice')

-- Test case 8:
testCreateCompletionChoice :: TestTree
testCreateCompletionChoice = testCase "Create a valid Choice." $ do
  let expectedChoice = Choice "" 0 Nothing ""
      actualChoice = createCompletionChoice 
  assertEqual "Test 8: Choice created should match expected Choice value" expectedChoice actualChoice
