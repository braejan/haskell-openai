{-#LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Chat.ChatGPT3.Helper where
import Data.Text (Text)
import OpenAI.API.V1.Chat.ChatGPT3.Instruction (rememberPreviousReply)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import System.Console.ANSI (setSGRCode, SGR (SetColor, Reset), ConsoleLayer (Foreground), ColorIntensity (Vivid), Color (Red))
import Control.Monad.IO.Class (MonadIO(liftIO))

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
  [ (":wm:", "What means \"{input}\"?")
  , (":ws:", "What is \"{input}\"?")
  , (":ts:", "Translate the next word or sentence to Spanish 🇪🇸: {input}")
  , (":te:", "Translate the next word or sentence to English 🇺🇸: {input}")
  , (":dyn:", "Do you know what is {input}?")
  , (":rw:", "Resume the next website: {input}")
  , (":rws:", "Resume the next website and translate it to Spanish 🇪🇸: {input}")
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
        input = T.dropWhile (/= ' ') original
        searchResult = HashMap.lookup command commands
        commandToReplace = fromMaybe original searchResult
        isTheOriginal = commandToReplace == original
        replacedCommand = T.replace "{input}" input commandToReplace
    in
        (if isTheOriginal then original else replacedCommand)
