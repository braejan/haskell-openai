module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.Types.RequestTest (allRequestTest)
import OpenAI.Types.ChoiceTest (allChoiceTest)
import OpenAI.Types.UsageTest (allUsageTest)
import OpenAI.Types.ResponseTest (allResponseTest)

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ allRequestTest
     ,allChoiceTest
     ,allUsageTest
     ,allResponseTest
    ]

main :: IO ()
main = defaultMain tests