module GPT3.Types.OpenAI.RequestTest where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack)
import GHC.Generics
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import GPT3.Types.OpenAI.Request (OpenAIRequest(..))


-- Test suite definition
allRequestTest :: TestTree
allRequestTest =
  testGroup "Test suite for Module gpt3-types: Request"
    [ testSerializationAnDeserialization -- Test case defined earlier
    ]

-- Test case 1: Serialize and deserialize a User value
testSerializationAnDeserialization :: TestTree
testSerializationAnDeserialization =
  testCase "Serialize and deserialize a OpenAIRequest JSON" $ do
    let user = OpenAIRequest (pack "text-davinci-003") (pack "Say this is a test") 0.0 7
        json = encode user
        user' = decode json
    assertEqual "Deserialized value should match original value" (Just user) user'