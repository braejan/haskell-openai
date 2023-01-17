{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified OpenAI.API.V1.Completion.Request as CRequest
import qualified OpenAI.API.V1.Completion.Response as CResponse
import qualified OpenAI.API.V1.Completion.Client as CClient
import OpenAI.API.V1.Edit.Code (editCode)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Completion.Choice as CChoice
import OpenAI.API.V1.Util.EditUtil (getTextFromChoices)
import qualified OpenAI.API.V1.Edit.Response as EResponse
import OpenAI.API.V1.Edit.Text (editText)

main :: IO ()
main = do 
    callCompletion
    callEditCode
    callEditText

callEditCode :: IO ()
callEditCode = do
    putStrLn $ "\n\n\n\n\n"
    putStrLn "Example calling a Edit Haskell code:"
    putStrLn "***** The Code *****"
    let code = "getTextFromChoices :: [Choice] -> Text\n\
               \getTextFromChoices choices = do\n\
               \    let texts = map text choices\n\
               \    T.intercalate \"\n\" texts\n"
    putStrLn $ T.unpack code
    let instruction = "Optimize this Haskell method"
    putStrLn $ T.unpack ("Instruction : " <> instruction)
    result <- editCode code instruction
    putStrLn "***** The Result *****"
    putStrLn $ T.unpack result

callEditText :: IO ()
callEditText = do
    putStrLn "Example calling a Edit text:"
    putStrLn "***** The Text *****"
    let text =   "GPT-3 is a very nice AI\
                \ That's pretty good at writing replies\
                \ When it's asked a question\
                \ It gives its suggestion\
                \ This is a poem it made that rhymes"
    putStrLn $ T.unpack text
    let instruction = "Make this in the voice of GPT-3"
    putStrLn $ T.unpack ("Instruction : " <> instruction)
    result <- editText text instruction
    putStrLn "***** The Result *****"
    putStrLn $ T.unpack result


callCompletion :: IO()
callCompletion = do
    print "Example calling a text-davinci-003 model with multiple actions"
    let rq = CRequest.createEmptyRequest {
        CRequest.model = "text-davinci-003",
        CRequest.prompt = Just $ Right [
            "Create a motivation message to encourage people to learn Haskell.",
            "Create a disclaimer about learning Haskell can be hard."
        ],
        CRequest.topP = Just 0.5,
        CRequest.maxTokens = Just 256
    }
    result <- CClient.create rq
    let getTextFromChoices = T.intercalate "\n" . map CChoice.text
    case result of
        Left error -> print error
        Right response -> putStrLn . T.unpack $ getTextFromChoices (CResponse.choices response)