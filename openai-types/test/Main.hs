module Main(main) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import OpenAI.Types.RequestTest
import OpenAI.Types.ChoiceTest

-- Test suite definition
tests :: TestTree
tests =
  testGroup "Test suite for Module openai-types"
    [ allRequestTest
     ,allChoiceTest
    ]

main :: IO ()
main = defaultMain tests