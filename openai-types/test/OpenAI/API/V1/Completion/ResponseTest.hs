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
import OpenAI.API.V1.Completion.Choice (Choice, createCompletionChoice)
import OpenAI.API.V1.Common.Usage (Usage, createUsage)
import Data.String (fromString)
import qualified OpenAI.API.V1.Completion.Response as CR


-- Test suite definition
allCompletionResponseTest :: TestTree
allCompletionResponseTest =
  testGroup "Test suite for Module openai-types: Response"
    [ testEmptyDeSerialization --1
    , testEmptyString --2
    , testUntilId --3
    -- , testUntilObject --4
    -- , testUntilCreated --5
    -- , testUntilModel --6
    -- , testUntilChoices --7
    -- , testUntilUsage --8
    
    
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
testUntilId :: TestTree
testUntilId = testCase "Deserialization of a JSON with only id field and should be a Response with id field." $ do
  let expectedResponse = createResponse {CR.id = "test"}
      actualResponse = decode (fromString "{\"id\":\"test\"}") :: Maybe Response
  assertEqual "Test 3: Deserialized value should be a Response with id field." (Just expectedResponse) actualResponse
