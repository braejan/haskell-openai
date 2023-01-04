module OpenAI.API.V1.Completion.UsageTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.API.V1.Completion.Usage(Usage(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)


jsonString :: String
jsonString = "{\"prompt_tokens\":5,\"completion_tokens\":7,\"total_tokens\":12}"

-- Test suite definition
allUsageTest :: TestTree
allUsageTest =
  testGroup "Test suite for Module openai-types: Usage"
    [ testSerializationAnDeserialization,
      testStringSerialization,
      testEmptyStringSerialization,
      testDeSerialization
    ]

-- Test case 1:
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a Usage JSON" $ do
    let expected = Usage 5 7 12
        json = encode expected
        usage' = decode json
    assertEqual "1=>Deserialized value should match original value" (Just expected) usage'

-- Test case 2:
testStringSerialization :: TestTree
testStringSerialization = testCase "Serialization of String value" $ do
  let json = BS.pack jsonString
      expected = Right createDefaultUsage
      actual = eitherDecode json :: Either String Usage
  assertEqual "2=>Parsed value should match expected value" expected actual

-- Test case 3:
testEmptyStringSerialization :: TestTree
testEmptyStringSerialization = testCase "Serialization of a Empty string value" $ do
  let json = BS.pack ""
      actual = eitherDecode json :: Either String Usage
  assertBool "3=>Parsed value should match expected value" (isLeft actual)


-- Test case 4:
testDeSerialization :: TestTree
testDeSerialization = testCase "Deserialization of a default Usage test to String." $ do
  let expected = BS.pack jsonString
  let usage = createDefaultUsage
  let actual = encode usage
  assertEqual "4=>Parsed value should match expected value" expected actual


-- | Create a new 'Usage' value with the given parameters
createUsage :: Int -> Int -> Int -> Usage
createUsage pt ct tt
  | pt /= 0 = Usage pt ct tt
  | otherwise =  Usage 5 7 12


-- | Create a new 'Usage' value with default test values
createDefaultUsage :: Usage
createDefaultUsage = createUsage 0 0 0

-- | Create a new 'Usage' value with default test values
createEmptyUsage :: Usage
createEmptyUsage = Usage 0 0 0