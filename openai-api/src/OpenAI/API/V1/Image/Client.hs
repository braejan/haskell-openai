{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Image.Client where
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Common.Const as Const
import OpenAI.API.V1.Image.Request (Request)
import OpenAI.API.V1.Image.Response (Response)
import OpenAI.API.V1.Common.Configuration
import Data.Aeson (encode)
import Network.Wreq (postWith, asJSON, responseStatus, responseBody, statusCode, statusMessage)
import Control.Lens ((^.))
about :: Text
about = "Given a prompt and/or an input image, the model will generate a new image."

-- | url display the final url for completions.
url :: String
url = T.unpack $ Const.baseURL <> "images/"
-- | createImage Creates an image given a prompt.
createImage :: Request -> IO(Either Text Response)
createImage request = do
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
    let requestBody = encode request
    let finalUrl = url <> "generations"
    rsp <- asJSON =<< postWith headers finalUrl requestBody
    let status = rsp ^. responseStatus . statusCode
    let response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)