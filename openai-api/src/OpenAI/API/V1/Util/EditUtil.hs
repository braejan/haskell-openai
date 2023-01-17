{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Util.EditUtil where
import Data.Text (Text, intercalate)
import OpenAI.API.V1.Edit.Choice (Choice(text))


getTextFromChoices :: [Choice] -> Text
getTextFromChoices = intercalate "\n" . map text