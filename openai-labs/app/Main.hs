{-#LANGUAGE OverloadedStrings #-}
module Main where
import System.Random (randomRIO)
import Data.Text

import qualified Data.Text.IO as TIO
import OpenAI.API.V1.Completion.Request (Request(..), completionRequest)
import OpenAI.API.V1.Completion.Response (Response (choices))
import OpenAI.API.V1.Completion.Client (createCompletion)
import Control.Monad (forM_)
import OpenAI.API.V1.Completion.Choice (Choice(text))
import OpenAI.API.V1.Chat.ChatGPT3.Chat (chatBot)


main :: IO ()
main = do
  putStrLn "Hello from openai-labs App"
  chatBot