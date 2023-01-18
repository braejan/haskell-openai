{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Model.Client where
import OpenAI.API.V1.Model.Model (Model)
import OpenAI.API.V1.Model.Response (Response(models))
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Common.Const as Const
import OpenAI.API.V1.Common.Configuration (fromEnvVariables, getHeaders)
import Network.Wreq (getWith, asJSON, responseStatus, statusCode, responseBody, statusMessage)
import Control.Lens ((^.))
import Data.Aeson (fromJSON, encode)

-- | OpenAI API description
about :: Text
about = "List and describe the various models available in the API. \
        \You can refer to the Models documentation to understand what \
        \models are available and the differences between them."

-- | url display the final url for completions.
url :: String
url = T.unpack $ Const.baseURL <> "models"

-- | Lists the currently available models, and provides basic information 
-- | about each one such as the owner and availability.
listModels :: IO (Either Text [Model])
listModels = do
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
    rsp <- asJSON =<< getWith headers url
    print rsp
    let status = rsp ^. responseStatus . statusCode
    let response = rsp ^. responseBody
    case status of
        200 -> pure $ Right (models response)
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)

-- | Retrieves a model instance, providing basic information about the 
-- | model such as the owner and permissioning.
retrieveModel :: Text -> IO(Either Text Model)
retrieveModel model = do
    let finalUrl = url <> "/" <> T.unpack model
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
    rsp <- asJSON =<< getWith headers finalUrl
    let status = rsp ^. responseStatus . statusCode
    let response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)