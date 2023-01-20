{-# LANGUAGE OverloadedStrings #-}
module Main where
import OpenAI.API.V1.Completion.Client (createCompletion)
import OpenAI.API.V1.Completion.Request (completionRequest, Request (..))
import qualified Data.Text as T
import OpenAI.API.V1.Model.Client (listModels, retrieveModel)
import OpenAI.API.V1.Edit.Client (createEdit)
import OpenAI.API.V1.Edit.Request (editRequest, Request (input, instruction, model))
import qualified OpenAI.API.V1.Completion.Request as CR
import qualified OpenAI.API.V1.Edit.Request as ER
import qualified OpenAI.API.V1.Image.Request as IR
import qualified OpenAI.API.V1.Image.Edit.Request as IER
import OpenAI.API.V1.Image.Client (createImage, createImageEdit)
import OpenAI.API.V1.Image.Request (imageRequest, Request (..))
import OpenAI.API.V1.Image.Edit.Request (imageEditRequest)
main :: IO ()
main = createImageEditSample

createCompletionSample :: IO()
createCompletionSample = do
    result <- createCompletion completionRequest{
        CR.model = "text-davinci-003",
        CR.prompt = Just $ Left "Say this is a test",
        maxTokens = Just 256
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response


listModelSample :: IO()
listModelSample = do
    result <- listModels
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response


retrieveModelSample :: IO()
retrieveModelSample = do
    result <- retrieveModel "text-davinci-003"
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response


createEditSample :: IO()
createEditSample = do
    result <- createEdit editRequest {
        ER.model = "text-davinci-edit-001",
        ER.input = Just "GPT-3 is a very nice AI\
                \That's pretty good at writing replies\
                \When it's asked a question\
                \It gives its suggestion\
                \This is a poem it made that rhymes",
        ER.instruction = "Make this in the voice of GPT-3"
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response

createImageSample :: IO()
createImageSample = do
    result <- createImage imageRequest {
        IR.prompt = "A cute baby sea otter",
        IR.n = Just 2,
        IR.size = Just "1024x1024"
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response


createImageEditSample :: IO()
createImageEditSample = do
    result <- createImageEdit imageEditRequest {
        IER.image = "otter.png",
        IER.mask = Just "mask.png",
        IER.prompt = "Remove the persons and replace it with Pandas",
        IER.n = Just 3,
        IER.size = Just "1024x1024"
    }
    case result of
        Left error -> putStrLn $ T.unpack ("😰 Error: \n" <> error)
        Right response -> putStrLn $ "🫡 Response: \n" <> show response