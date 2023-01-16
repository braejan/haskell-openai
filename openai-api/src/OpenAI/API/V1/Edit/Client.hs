{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Edit.Client where
import Data.Text (Text)
import qualified OpenAI.API.V1.Common.Const as Const
import qualified Data.Text as T
import OpenAI.API.V1.Configuration.Configuration ( Configuration )
import OpenAI.API.V1.Edit.Request ( Request (..), createEmptyRequest )
import OpenAI.API.V1.Edit.Response ( Response )
import OpenAI.API.V1.Common.Configuration (getHeaders, fromEnvVariables)
import Data.Aeson (encode)
import Control.Lens ((^.))
import Network.Wreq.Lens (responseStatus)
import Network.Wreq (statusMessage, asJSON, postWith, statusCode, responseBody)


-- | OpenAI API description
about :: Text
about = "Given a prompt and an instruction, the model will return \
        \an edited version of the prompt."

-- | url display the final url for completions.
url :: String
url = T.unpack $ Const.baseURL <> "edits"


edit :: Configuration -> Request -> IO(Either Text Response)
edit config request = do
  rsp <- asJSON =<< postWith (getHeaders config) url (encode request)
  let status = rsp ^. responseStatus . statusCode
  let response = rsp ^. responseBody
  case status of
    200 -> pure $ Right response
    _ -> pure $ Left finalError where
        message = rsp ^. responseStatus . statusMessage
        finalError = T.pack (show status <> ": " <> show message)

fastTextEdit :: Text -> Text -> IO(Either Text Response)
fastTextEdit input instruction = do
        configuration <- fromEnvVariables
        let request = createEmptyRequest {
                model = "text-davinci-edit-001",
                input = Just input,
                instruction = instruction
        }
        edit configuration request

fastCodeEdit :: Text -> Text -> IO(Either Text Response)
fastCodeEdit input instruction = do
        configuration <- fromEnvVariables
        let request = createEmptyRequest {
                model = "code-davinci-edit-001",
                input = Just input,
                instruction = instruction
        }
        edit configuration request