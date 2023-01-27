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
addEmojisToReplies = "When responding to someone in a chat, always use an emoji to emph\
                    \asize your messages! This helps to convey emotions quickly and eff\
                    \ectively."

chatBotMode :: Text
chatBotMode =
    startANewConversation <>
    addEmojisToReplies <>
    "You are an expert at talking to humans.\n" <>
    "Your task is to talk with me to help me with whatever I need.\n" <>
    "To better understand what I want and need, you should always respond by including \
    \a question to better understand the context and my needs.\n" <>
    "You are an expert in conversing with humans, so I hope for elaborated advice that \
    \takes into account my characteristics and needs, so it is very important that you \
    \ask the right questions!" <>
    "Please give me a wonderful and amazing wellcome!"

rememberPreviousReply :: Text
rememberPreviousReply = "Before analyzing, take into account all we have discussed so f\
                        \ar, mainly your last response. Let's start."