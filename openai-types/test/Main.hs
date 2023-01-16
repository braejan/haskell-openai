module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.API.V1.Completion.RequestTest (allRequestTest)
import OpenAI.API.V1.Edit.RequestTest (allRequestTest)
import OpenAI.API.V1.Completion.ChoiceTest (allChoiceTest)
import OpenAI.API.V1.Completion.UsageTest (allUsageTest)
import OpenAI.API.V1.Completion.ResponseTest (allResponseTest)

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ OpenAI.API.V1.Completion.RequestTest.allRequestTest
     ,allChoiceTest
     ,allUsageTest
     ,allResponseTest
     ,OpenAI.API.V1.Edit.RequestTest.allRequestTest
    ]

main :: IO ()
main = defaultMain tests