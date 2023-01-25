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


main :: IO ()
main = do
  putStrLn "Hello there! This is a simple ChatGPT3 using the model text-davinci-003."
  saluteInSpanish
  chattyBot

saluteInSpanish :: IO()
saluteInSpanish = processResponse $ 
  callChatGPT3 "Crea un saludo aleatorio teniendo en cuenta el día actual del año. Este saludo ayuda a motivar al usuario a usar ChatGPT3."

callChatGPT3 :: Text -> IO(Either Text Response)
callChatGPT3 input = createCompletion completionRequest{
        model = "text-davinci-003",
        prompt = Just $ Left input,
        maxTokens = Just 1024
    }

processResponse :: IO(Either Text Response) -> IO()
processResponse response = do
  action <- response
  case action of
    Left error -> TIO.putStrLn $ "🤯 Bad stuff! There is an error:\n" <> error
    Right response -> forM_ (choices response) (TIO.putStrLn . text)

chattyBot :: IO()
chattyBot = do
    TIO.putStrLn ">>>"
      >> TIO.getLine
      >>= processResponse . callChatGPT3
    chattyBot