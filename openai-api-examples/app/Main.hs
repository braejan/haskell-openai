{-# LANGUAGE OverloadedStrings #-}
module Main where
import OpenAI.API.V1.Completion.Client (createCompletion)
import OpenAI.API.V1.Completion.Request (completionRequest, Request (..))
import qualified Data.Text as T
main :: IO ()
main = do
    result <- createCompletion completionRequest{
        model = "text-davinci-003",
        prompt = Just $ Left "Say this is a test",
        maxTokens = Just 256
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response

completionSample :: IO()
completionSample = do
    result <- createCompletion completionRequest{
        model = "text-davinci-003",
        prompt = Just $ Left "Say this is a test",
        maxTokens = Just 256
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response
