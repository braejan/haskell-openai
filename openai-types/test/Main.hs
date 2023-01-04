module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.API.V1.Completion.RequestTest (allRequestTest)
import OpenAI.API.V1.Completion.ChoiceTest (allChoiceTest)
import OpenAI.API.V1.Completion.UsageTest (allUsageTest)
import OpenAI.API.V1.Completion.ResponseTest (allResponseTest)

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ --allRequestTest
     allChoiceTest
     --,allUsageTest
     --,allResponseTest
    ]

main :: IO ()
main = defaultMain tests