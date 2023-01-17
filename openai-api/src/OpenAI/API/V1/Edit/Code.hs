{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.Code where
import Data.Text (Text)
import OpenAI.API.V1.Configuration.Configuration (Configuration(..))
import OpenAI.API.V1.Common.Configuration (fromEnvVariables)
import OpenAI.API.V1.Edit.Request (Request(..), createEmptyRequest)
import qualified OpenAI.API.V1.Edit.Client as Client
import OpenAI.API.V1.Util.EditUtil (getTextFromChoices)
import OpenAI.API.V1.Edit.Response (Response(choices))
import qualified Data.Text as T
import OpenAI.API.V1.Edit.Choice (Choice(text))

defaultEditCodeModel :: Text
defaultEditCodeModel = "code-davinci-edit-001"

editCode :: Text -> Text -> IO Text
editCode code instruction = do
    configuration <- fromEnvVariables
    editCodeWithConfig configuration code instruction

editCodeWithConfig :: Configuration -> Text -> Text -> IO Text
editCodeWithConfig configuration code instruction = do
    let request = createEmptyRequest {
        model = defaultEditCodeModel,
        input = Just code,
        instruction = instruction
    }
    let fullText = T.intercalate "\n" . map text
    call <- Client.editWithConf configuration request
    case call of
      Left error -> return $ "Error: " <> error
      Right response -> return $ getTextFromChoices (choices response)
