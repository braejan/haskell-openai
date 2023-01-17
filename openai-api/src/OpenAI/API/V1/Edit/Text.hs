{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.Text where
import Data.Text (Text)
import OpenAI.API.V1.Configuration.Configuration (Configuration(..))
import OpenAI.API.V1.Common.Configuration (fromEnvVariables)
import OpenAI.API.V1.Edit.Request (Request(..), createEmptyRequest)
import qualified OpenAI.API.V1.Edit.Client as Client
import OpenAI.API.V1.Util.EditUtil (getTextFromChoices)
import OpenAI.API.V1.Edit.Response (Response(choices))
import qualified Data.Text as T
import qualified OpenAI.API.V1.Edit.Choice as C

defaultEditTextModel :: Text
defaultEditTextModel = "text-davinci-edit-001"

editText :: Text -> Text -> IO Text
editText text instruction = do
    configuration <- fromEnvVariables
    editTextWithConfig configuration text instruction

editTextWithConfig :: Configuration -> Text -> Text -> IO Text
editTextWithConfig configuration text instruction = do
    let request = createEmptyRequest {
        model = defaultEditTextModel,
        input = Just text,
        instruction = instruction
    }
    let fullText = T.intercalate "\n" . map C.text
    call <- Client.editWithConf configuration request
    case call of
      Left error -> return $ "Error: " <> error
      Right response -> return $ fullText (choices response)
