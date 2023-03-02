{-#LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Chat.ChatGPT3.Helper where
import Data.Text (Text)
import OpenAI.API.V1.Chat.ChatGPT3.Instruction (rememberPreviousReply)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)

keepTheConversation :: Maybe (Either Text [Text]) -> Maybe (Either Text [Text])
keepTheConversation input =
    case input of
        Nothing -> Nothing
        Just eitherTextOrArray -> Just $
            keepTheConversationOnEither eitherTextOrArray


keepTheConversationOnEither :: Either Text [Text] -> Either Text [Text]
keepTheConversationOnEither input =
    case input of
        Left text -> Left $ keepTheConversationOnText text
        Right array -> Right $ keepTheConversationOnTextArray array

keepTheConversationOnText :: Text -> Text
keepTheConversationOnText input = rememberPreviousReply <> input


keepTheConversationOnTextArray :: [Text] -> [Text]
keepTheConversationOnTextArray array = rememberPreviousReply : array

finalInput :: Text -> Text
finalInput original =
    replaceCommand original <>
    "<|endoftext|>"

commands :: HashMap.HashMap Text Text
commands = HashMap.fromList
  [ (":es:", "En este Chat vamos a hablar en Español. Ten siempre presente que en este Chat estamos usando el idioma Español. Por favor, no uses palabras en otro idioma. Si quieres usar otro idioma, por favor, usa el Chat en Inglés. Gracias.")
  , (":wm:", "What means the next word or sentence?\nWord or sentence: ")
  , (":ts:", "Translate the next word or sentence to Spanish: ")
  , (":te:", "Translate the next word or sentence to English: ")
  ]
-- use the commands HashMap to lookup all commands and print how to use them.
showAllCommands :: IO()
showAllCommands = do
    let commandsList = HashMap.toList commands
    putStrLn "Commands:"
    mapM_ (\(command, description) -> putStrLn $ T.unpack command <> " " <> T.unpack description) commandsList

replaceCommand :: Text -> Text
replaceCommand original =
    let
        command = T.takeWhile (/= ' ') original
        searchResult = HashMap.lookup command commands
        commandToReplace = fromMaybe original searchResult
        isTheOriginal = commandToReplace == original
        replacedCommand = T.replace command commandToReplace original
    in
        (if isTheOriginal then original else replacedCommand)
