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
  [ (":spanish:", "Translate to Spanish the next text: ")
  , (":explain:", "Explain the next English word or English phrase in Spanish:")
  ]



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
