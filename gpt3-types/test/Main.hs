module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import GPT3.Types.OpenAI.RequestTest

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module gpt3-types"
    [ allRequestTest
    ]

main :: IO ()
main = defaultMain tests