{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Util.Completion.ChoiceUtil where
import OpenAI.API.V1.Completion.Choice (Choice (text))
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)
import Data.Text (strip)


showAllTextFromChoices ::[Choice] -> IO()
showAllTextFromChoices choices = forM_ choices printTrimChoiceText

printTrimChoiceText :: Choice -> IO ()
printTrimChoiceText  = TIO.putStrLn . ("🤖🤖🤖🤖🤖 " <> ) . strip . text

printChoiceText :: Choice -> IO ()
printChoiceText  = TIO.putStrLn . text