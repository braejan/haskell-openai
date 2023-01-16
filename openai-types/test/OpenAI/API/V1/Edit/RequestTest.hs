{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.RequestTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Edit.Request(Request(..), createEmptyRequest)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map

jsonString :: String
jsonString = "{\"model\":\"text-davinci-002\",\"input\":\"The quick brown fox jumps over the lazy dog.\",\"instruction\":\"Make the sentence more interesting\",\"n\":3,\"temperature\":0.5,\"top_p\":0.8}"

jsonStringMandatory :: String
jsonStringMandatory = "{\"model\":\"text-davinci-002\",\"instruction\":\"Make the sentence more interesting\"}"

-- Test suite definition
allRequestTest :: TestTree
allRequestTest =
  testGroup "Test suite for Module Edits in openai-types: Request"
    [ testSerializationAnDeserialization,
      testStringSerialization,
      testEmptyStringSerialization,
      testDeSerialization,
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


-- Test case 6:
testDeSerializationMandatory :: TestTree
testDeSerializationMandatory = testCase "Deserialization of a default Request test to String." $ do
  let expected = BS.pack jsonStringMandatory
  let request = createEmptyRequest {
    model = "text-davinci-002",
    instruction = "Make the sentence more interesting"
  }
  let actual = encode request
  assertEqual "6=>Parsed value should match expected value" expected actual

-- | Create a new 'Request' value with default test values
createDefaultRequest :: Request
createDefaultRequest = Request
  { model = pack "text-davinci-002"
  , input = Just "The quick brown fox jumps over the lazy dog."
  , instruction = "Make the sentence more interesting"
  , n = Just 3
  , temperature = Just 0.5
  , topP = Just 0.8
  }
