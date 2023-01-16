
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified OpenAI.API.V1.Completion.Client as Client
import OpenAI.API.V1.Completion.Request (Request(..), createEmptyRequest)

main :: IO ()
main = do
    --Calling using a custom configuration:
    let request = createEmptyRequest {
        model = "your-favorite-model",
        prompt = Just $ Left "my custom prompt"
    --  to use an array you should use:
    --  prompt = Just $ Rigth ["custom prompt 0", "custom prompt n"]
    }
    call <- Client.fastCompletion request
    case call of
      Left error -> print ("call with error: " <> error)
      Right response -> print response 

    