module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.API.V1.Completion.RequestTest (allRequestTest)
import OpenAI.API.V1.Edit.RequestTest (allRequestTest)
import OpenAI.API.V1.Completion.ChoiceTest (allChoiceTest)
import OpenAI.API.V1.Common.UsageTest (allUsageTest)
import OpenAI.API.V1.Completion.ResponseTest (allResponseTest)
import OpenAI.API.V1.Edit.ResponseTest (allResponseTest)
import qualified OpenAI.API.V1.Edit.ChoiceTest

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
    ]

main :: IO ()
main = defaultMain tests