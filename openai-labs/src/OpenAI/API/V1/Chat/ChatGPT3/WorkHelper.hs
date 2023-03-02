{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Chat.ChatGPT3.WorkHelper where
import OpenAI.API.V1.Completion.Request (Request (model, maxTokens, topP, presencePenalty, frequencyPenalty, bestOf, stop, prompt), completionRequest)
import qualified Data.Text.IO as TIO
import Control.Monad (forever)
import OpenAI.API.V1.Completion.Client (createCompletion)
import OpenAI.API.V1.Chat.ChatGPT3.Instruction (chatBotMode, workerHelperWellcome, showModelAndURLInstruction)
import qualified Data.Text as T
import OpenAI.API.V1.Util.Completion.ChoiceUtil (showAllTextFromChoices)
import OpenAI.API.V1.Util.Completion.ResponseUtil (getChoicesFromResponse)
import Data.Text (Text)
import OpenAI.API.V1.Completion.Response (Response)
import Data.Char (chr)
import OpenAI.API.V1.Chat.ChatGPT3.Helper (finalInput)
import Data.Either (fromLeft)
import qualified Data.Text.IO as T

modelToUse :: Text
modelToUse = "text-davinci-003"

urlModel :: Text
urlModel = "https://platform.openai.com/docs/models/gpt-3-5"

completionRequestChatBot :: Request
completionRequestChatBot = completionRequest {
    model = modelToUse,
    maxTokens = Just 1024,
    topP = Just 1,
    presencePenalty = Just 0.6,
    frequencyPenalty = Just 0.1,
    bestOf = Just 3,
    stop = Just $ Left "stop"
}

callCreateCompletion :: Either Text [Text] -> Request -> IO(Either Text Response)
callCreateCompletion input request = do
    let inputToSend = case input of
            Left input -> Left $ finalInput input
            Right input -> Right $ map finalInput input
    createCompletion request { prompt = Just inputToSend }

-- display the response
talk :: Request -> Either Text [Text] -> IO ()
talk request input = do
    result <- callCreateCompletion input request
    case result of
        Left error -> putStrLn $ "Error: " <> show error
        Right response -> showAllTextFromChoices $  getChoicesFromResponse response
-- start the chatbot
chatBot ::IO()
chatBot = do
    T.putStrLn workerHelperWellcome
    talk completionRequestChatBot $ Left $ showModelAndURLInstruction modelToUse urlModel
    putStrLn "📝📝📝📝📝 Write to chatbot 📝📝📝📝📝"
    forever $ do
        putStrLn "📝📝📝📝📝"
        input <- getLine
        talk completionRequestChatBot $ Left $ T.pack input