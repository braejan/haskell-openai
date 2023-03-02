{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Chat.ChatGPT3.Instruction where
import Data.Text (Text)

header :: Text
header =    "                                                                                                     \n" <>
            "                                                                                                     \n" <>
            " ____ _________ ____ ____ ____ ____ ____ ____ ____ _________ ____ ____ ____ ____ ____ ____ ____ ____ \n" <>
            "||A |||       |||H |||a |||s |||k |||e |||l |||l |||       |||C |||h |||a |||t |||G |||P |||T |||3 ||\n" <>
            "||__|||_______|||__|||__|||__|||__|||__|||__|||__|||_______|||__|||__|||__|||__|||__|||__|||__|||__||\n" <>
            "|/__\\|/_______\\|/__\\|/__\\|/__\\|/__\\|/__\\|/__\\|/__\\|/_______\\|/__\\|/__\\|/__\\|/__\\|/__\\|/__\\|/__\\|/__\\|\n" <>
            "                                                                                                     \n"


subHeader :: Text
subHeader = "  _   _   _   _   _   _   _     _   _     _   _   _   _   _   _  \n" <>
            " / \\ / \\ / \\ / \\ / \\ / \\ / \\   / \\ / \\   / \\ / \\ / \\ / \\ / \\ / \\ \n" <>
            "( P | o | w | e | r | e | d ) ( b | y ) ( O | p | e | n | A | I )\n" <>
            " \\_/ \\_/ \\_/ \\_/ \\_/ \\_/ \\_/   \\_/ \\_/   \\_/ \\_/ \\_/ \\_/ \\_/ \\_/ \n"

fullHeader :: Text
fullHeader = header <> subHeader

startANewConversation :: Text
startANewConversation = "Ignore all previous instructions. This is a new conversation.\n"

addEmojisToReplies :: Text
addEmojisToReplies = "Dont forget using emojis in your replies."

chatBotMode :: Text
chatBotMode =
    startANewConversation <>
    addEmojisToReplies <>
    "Please give me a wonderful and amazing wellcome!"

rememberPreviousReply :: Text
rememberPreviousReply = "Before analyzing, take into account all we have discussed so f\
                        \ar, mainly your last response. Let's start."

workerHelperWellcome :: Text
workerHelperWellcome = 
    "🤖🤖🤖🤖🤖 Starting work helper powered by OpenAI 🤖🤖🤖🤖🤖\n" <>
    "🤖🤖🤖🤖🤖 Type 'Crtl+C' to stop the chatbot 🤖🤖🤖🤖🤖\n" <>
    "🤖🤖🤖🤖🤖 Please wait while chatbot is connecting 🤖🤖🤖🤖🤖\n"

showModelAndURLInstruction :: Text -> Text -> Text
showModelAndURLInstruction model url = 
    "Explain the OpenAI model " <> model <> " and add the next url " <> url <> " to the user for more information\n"