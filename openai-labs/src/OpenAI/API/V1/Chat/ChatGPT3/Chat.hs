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


header :: Text
header = " _______       ______  __             ______      ___________   ______________        ____________________________\n\
          \___    |      ___  / / /_____ __________  /_________  /__  /   __  ____/__  /_______ __  /__  ____/__  __ \\__  __/_|__  /\n\
          \__  /| |      __  /_/ /_  __ `/_  ___/_  //_/  _ \\_  /__  /    _  /    __  __ \\  __ `/  __/  / __ __  /_/ /_  /  ___/_ < \n\
          \_  ___ |      _  __  / / /_/ /_(__  )_  ,<  /  __/  / _  /     / /___  _  / / / /_/ // /_ / /_/ / _  ____/_  /   ____/ / \n\
          \/_/  |_|      /_/ /_/  \\__,_/ /____/ /_/|_| \\___//_/  /_/      \\____/  /_/ /_/\\__,_/ \\__/ \\____/  /_/     /_/    /____/  \n\n"


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

showEitherErorOrResponse :: IO(Either Text Response) -> IO()
showEitherErorOrResponse ioEither = do
    eitherErrorOrResponse <- ioEither 
    case eitherErrorOrResponse of
        Left error -> TIO.putStrLn $ 
            "🤯 I tried to wellcome to you, buuuuuuuuut:\n" <> 
            error <> 
            "\nTry asking something:"
        Right response -> showAllTextFromChoices $  getChoicesFromResponse response

salute :: IO()
salute = do
    let input = "Think you are the 'Amazing Haskell CLI Chatbot'. You're built using Haskell.\
                \This CLI can be used for everyone not only Haskell developers. Is created to\
                \ chat with you a OS terminal.\n\
                \The Haskell code can be found at https://github.com/braejan/haskell-openai.\
                \Now, Say hello to the new user who just connected! It should be a nice well\
                \come that invite the user to use you. Don't forget to use emojis in the message."
    showEitherErorOrResponse $ callChatGPT3 input

talk :: Text -> IO ()
talk = showEitherErorOrResponse . callChatGPT3

wellcome :: IO()
wellcome = do
    TIO.putStrLn header
    salute

chatBot :: IO()
chatBot = do
    wellcome
    forever $ do
        putStr "📝"
          >> do
            putStr "💬 "
            TIO.getLine
          >>= talk
