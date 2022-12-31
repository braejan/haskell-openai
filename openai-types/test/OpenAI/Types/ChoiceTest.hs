module OpenAI.Types.ChoiceTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text(pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import OpenAI.Types.Choice(Choice(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.Either (isLeft)


-- Test suite definition
allChoiceTest :: TestTree
allChoiceTest =
  testGroup "Test suite for Module openai-types: Choice"
    [ testSerializationAnDeserialization,
      testStringSerialization
    ]

-- Test case 1:
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a Choice JSON" $ do
    let expected = Choice (pack "This is indeed a test") 0 (Just []) (pack "")
        json = encode expected
        choice' = decode json
    assertEqual "1=>Deserialized value should match original value" (Just expected) choice'

-- Test case 2:
testStringSerialization :: TestTree
testStringSerialization = testCase "Serialization of String value" $ do
  let json = BS.pack "{\"text\": \"This is indeed a test\", \"index\": 0, \"logprobs\": null, \"finish_reason\": \"length\"}"
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
  let expected = BS.pack "{\"text\": \"This is indeed a test\", \"index\": 0, \"logprobs\": null, \"finish_reason\": \"length\"}"
  let choice = createDefaultChoice
  let actual = encode choice
  assertEqual "4=>Parsed value should match expected value" expected actual


-- | Create a new 'Choice' value with the given parameters
createChoice :: String -> Int -> Maybe [Double] -> String -> Choice
createChoice t i l fr
  | t /= "" = Choice (pack t) i l (pack fr)
  | otherwise =  Choice (pack "This is indeed a test") 0 Nothing (pack "length")


-- | Create a new 'Choice' value with default test values
createDefaultChoice :: Choice
createDefaultChoice = createChoice "" 0 Nothing ""

-- | Create a new 'Choice' value with default test values
createEmptyChoice :: Choice
createEmptyChoice = Choice (pack "") 0 (Just []) (pack "")