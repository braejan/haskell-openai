module OpenAI.API.V1.Chat.ChatGPT3.Helper where
import Data.Text (Text)
import OpenAI.API.V1.Chat.ChatGPT3.Instruction (rememberPreviousReply)

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
