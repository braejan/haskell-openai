{-#LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Chat.ChatGPT3.Chat where
import Data.Text (Text)
import OpenAI.API.V1.Completion.Response (Response)
import OpenAI.API.V1.Completion.Request (Request(..), completionRequest)
import OpenAI.API.V1.Completion.Client (createCompletion)
import qualified Data.Text.IO as TIO
import OpenAI.API.V1.Util.Completion.ChoiceUtil (showAllTextFromChoices)
import OpenAI.API.V1.Util.Completion.ResponseUtil (getChoicesFromResponse)
import Control.Monad (forever)
import OpenAI.API.V1.Chat.ChatGPT3.Instruction (chatBotMode, fullHeader)

-- create a completion request and call the OpenAI API to get the response
callChatGPT3 :: Text -> IO(Either Text Response)
callChatGPT3 input = createCompletion completionRequest{
        model = "text-davinci-003",
        prompt = Just $ Left input,
        maxTokens = Just 1024,
        temperature = Just 0.9,
        topP = Just 1.0,
        presencePenalty = Just 0.6,
        bestOf = Just 1
    }

-- display either an error message or the response
showEitherErorOrResponse :: IO(Either Text Response) -> IO()
showEitherErorOrResponse ioEither = do
    eitherErrorOrResponse <- ioEither 
    case eitherErrorOrResponse of
        Left error -> TIO.putStrLn $ 
            "🤯 I tried to wellcome to you, buuuuuuuuut:\n" <> 
            error <> 
            "\nTry asking something:"
        Right response -> showAllTextFromChoices $  getChoicesFromResponse response

-- display the response
talk :: Text -> IO ()
talk = showEitherErorOrResponse . callChatGPT3

-- start the chat with a chatbot mode
startChat :: IO()
startChat = do
    putStrLn "🚀 Starting..."
    talk chatBotMode

-- display the welcome message and start the chat
wellcome :: IO()
wellcome = do
    TIO.putStrLn fullHeader
    startChat

-- start the chatbot
chatBot :: IO()
chatBot = do
    wellcome
    forever $ do
        putStrLn "📝📝📝📝📝"
        TIO.getLine >>= talk
