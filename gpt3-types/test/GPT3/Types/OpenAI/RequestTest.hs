{-# LANGUAGE BlockArguments #-}
module GPT3.Types.OpenAI.RequestTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import GPT3.Types.OpenAI.Request (OpenAIRequest(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)


-- Test suite definition
allRequestTest :: TestTree
allRequestTest =
  testGroup "Test suite for Module gpt3-types: Request"
    [ testSerializationAnDeserialization,
      testStringSerialization
    ]

-- Test case 1:
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a OpenAIRequest JSON" $ do
    let expected = OpenAIRequest (pack "text-davinci-003") (pack "Say this is a test") 0.0 7
        json = encode expected
        request' = decode json
    assertEqual "1=>Deserialized value should match original value" (Just expected) request'

-- Test case 2:
testStringSerialization :: TestTree
testStringSerialization = testCase "Serialization of String value" $ do
  let json = BS.pack "{\"model\":\"text-davinci-003\",\"prompt\":\"Say this is a test\",\"temperature\":0,\"max_tokens\":7}"
      expected = Right createDefaultOpenAIRequest
      actual = eitherDecode json :: Either String OpenAIRequest
  assertEqual "2=>Parsed value should match expected value" expected actual

-- Test case 3:
testEmptyStringSerialization :: TestTree
testEmptyStringSerialization = testCase "Serialization of a Empty string value" $ do
  let json = BS.pack ""
      actual = eitherDecode json :: Either String OpenAIRequest
  assertBool "3=>Parsed value should match expected value" (isLeft actual)


-- Test case 4:
testDeSerialization :: TestTree
testDeSerialization = testCase "Deserialization of a default OpenAiRequest test to String." $ do
  let expected = BS.pack "{\"model\":\"text-davinci-003\",\"prompt\":\"Say this is a test\",\"temperature\":0,\"max_tokens\":7}"
  let request = createDefaultOpenAIRequest
  let actual = encode request
  assertEqual "4=>Parsed value should match expected value" expected actual


-- | Create a new 'OpenAIRequest' value with the given parameters
createOpenAIRequest :: String -> String -> Float -> Int -> OpenAIRequest
createOpenAIRequest m p t mt
  | m /= "" = OpenAIRequest (pack m) (pack p) t mt
  | otherwise =  OpenAIRequest (pack "text-davinci-003") (pack "Say this is a test") 0 7


-- | Create a new 'OpenAIRequest' value with default test values
createDefaultOpenAIRequest :: OpenAIRequest
createDefaultOpenAIRequest = createOpenAIRequest "" "" 0 0

-- | Create a new 'OpenAIRequest' value with default test values
createEmptyOpenAIRequest :: OpenAIRequest
createEmptyOpenAIRequest = OpenAIRequest (pack "") (pack "") 0 0