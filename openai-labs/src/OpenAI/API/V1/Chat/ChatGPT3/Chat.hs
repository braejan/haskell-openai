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
import qualified Data.Text as T
import Control.Exception (try, IOException, catch)

-- create a completion request and call the OpenAI API to get the response
callChatGPT3 :: Text -> Request -> IO(Either Text Response)
callChatGPT3 input request = do
    catch (createCompletion request { prompt = Just $ Left input })
        handleIOException

handleIOException :: IOException -> IO (Either Text Response)
handleIOException = pure . Left . T.pack . show

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

-- defaultRequest is a request with default parameters
defaultRequest :: Request
defaultRequest = completionRequest {
        model = "text-davinci-003",
        maxTokens = Just 1024,
        topP = Just 1,
        presencePenalty = Just 0.6,
        frequencyPenalty = Just 0.1,
        bestOf = Just 1
    }

-- display the response
talk :: Request -> Text -> IO ()
talk request input = do
    showEitherErorOrResponse $ callChatGPT3 input request

-- start the chat with a chatbot mode
startChat :: IO()
startChat = do
    putStrLn "🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 🚀 Starting..."
    talk defaultRequest chatBotMode

-- display the welcome message and start the chat
wellcome :: IO()
wellcome = do
    TIO.putStrLn fullHeader
    startChat

defaultChatBot :: IO()
defaultChatBot = do
    wellcome
    forever $ do
        putStrLn "📝📝📝📝📝"
        TIO.getLine >>= talk defaultRequest
-- start the chatbot
chatBot :: Request -> IO()
chatBot request = do
    wellcome
    forever $ do
        putStrLn "📝📝📝📝📝"
        TIO.getLine >>= talk request 

