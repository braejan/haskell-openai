{-#LANGUAGE OverloadedStrings #-}
module Main where
import OpenAI.API.V1.Chat.ChatGPT3.Chat (chatBot)
import OpenAI.API.V1.Completion.Request (Request(..), completionRequest)

main :: IO ()
main = do 
    let request = completionRequest {
            model = "text-davinci-003",
            maxTokens = Just 1024,
            topP = Just 1,
            presencePenalty = Just 0.6,
            frequencyPenalty = Just 0.1,
            bestOf = Just 1
        }
    chatBot request