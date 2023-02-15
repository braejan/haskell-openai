module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified OpenAI.API.V1.Common.UsageTest
import qualified OpenAI.API.V1.Completion.ChoiceTest
import qualified OpenAI.API.V1.Completion.RequestTest

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    -- [ OpenAI.API.V1.Completion.RequestTest.allRequestTest
    --  ,OpenAI.API.V1.Completion.ChoiceTest.allChoiceTest
    --  ,OpenAI.API.V1.Edit.ChoiceTest.allChoiceTest
    --  ,allUsageTest
    --  ,OpenAI.API.V1.Completion.ResponseTest.allResponseTest
    --  ,OpenAI.API.V1.Edit.ResponseTest.allResponseTest
    --  ,OpenAI.API.V1.Edit.RequestTest.allRequestTest
    -- ]
    [ OpenAI.API.V1.Common.UsageTest.allUsageTest
    , OpenAI.API.V1.Completion.ChoiceTest.allCompletionChoiceTest
    , OpenAI.API.V1.Completion.RequestTest.allCompletionRequestTest
    ]

main :: IO ()
main = defaultMain tests