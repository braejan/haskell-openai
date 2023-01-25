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


botHeader :: String
botHeader = " _______       ______  __             ______      ___________   ______________        ____________________________\n___    |      ___  / / /_____ __________  /_________  /__  /   __  ____/__  /_______ __  /__  ____/__  __ \\__  __/_|__  /\n__  /| |      __  /_/ /_  __ `/_  ___/_  //_/  _ \\_  /__  /    _  /    __  __ \\  __ `/  __/  / __ __  /_/ /_  /  ___/_ < \n_  ___ |      _  __  / / /_/ /_(__  )_  ,<  /  __/  / _  /     / /___  _  / / / /_/ // /_ / /_/ / _  ____/_  /   ____/ / \n/_/  |_|      /_/ /_/  \\__,_/ /____/ /_/|_| \\___//_/  /_/      \\____/  /_/ /_/\\__,_/ \\__/ \\____/  /_/     /_/    /____/  "
main :: IO ()
main = do
  putStrLn botHeader
  putStrLn "A Haskell CLI ChatBot using ChatGPT3.\n\n"
  saluteInSpanish
  chattyBot

saluteInSpanish :: IO()
saluteInSpanish = processResponse $ 
  callChatGPT3 "Crea un saludo aleatorio teniendo en cuenta el día actual del año. Este saludo ayuda a motivar al usuario a usar ChatGPT3."

callChatGPT3 :: Text -> IO(Either Text Response)
callChatGPT3 input = createCompletion completionRequest{
        model = "text-davinci-003",
        prompt = Just $ Left input,
        maxTokens = Just 1024,
        topP = Just 0.1
    }

processResponse :: IO(Either Text Response) -> IO()
processResponse response = do
  action <- response
  case action of
    Left error -> TIO.putStrLn $ "🤯 Bad stuff! There is an error:\n" <> error
    Right response -> forM_ (choices response) (TIO.putStrLn . strip . text)

chattyBot :: IO()
chattyBot = do
    TIO.putStr ">>>"
      >> TIO.getLine
      >>= processResponse . callChatGPT3
    chattyBot