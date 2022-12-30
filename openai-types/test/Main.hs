module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.Types.RequestTest

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ allRequestTest
    ]

main :: IO ()
main = defaultMain tests