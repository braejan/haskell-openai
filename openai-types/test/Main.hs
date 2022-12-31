module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.Types.RequestTest
import OpenAI.Types.ChoiceTest
import OpenAI.Types.UsageTest

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ allRequestTest
     ,allChoiceTest
     ,allUsageTest
    ]

main :: IO ()
main = defaultMain tests