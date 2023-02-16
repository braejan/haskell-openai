{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.Client where
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Common.Const as Const
import OpenAI.API.V1.Completion.Request (Request)
import OpenAI.API.V1.Completion.Response (Response)
import OpenAI.API.V1.Common.Configuration (fromEnvVariables, getHeaders)
import Data.Aeson (encode)
import Network.Wreq (asJSON, responseStatus, statusCode, statusMessage, responseBody, postWith)
import Control.Lens ((^.))
import Data.Text.Encoding (decodeUtf8)
-- | OpenAI API description
about :: Text
about = "Given a prompt, the model will return one or more predicted \
         \completions, and can also return the probabilities of alter \
         \native tokens at each position."

-- | url display the final url for completions.
url :: String
url = T.unpack $ Const.baseURL <> "completions"


-- | createCompletion is a method that takes the configuration from ENV variables and create send a request.
createCompletion :: Request -> IO (Either Text Response)
createCompletion request = do
  configuration <- fromEnvVariables
  let headers = getHeaders configuration
      requestBody = encode request
  rsp <- asJSON =<< postWith headers url requestBody
  let status = rsp ^. responseStatus . statusCode
      message = rsp ^. responseStatus . statusMessage
      response = rsp ^. responseBody
  case status of
    200 -> pure (Right response)
    _   -> pure (Left $ T.intercalate ": " [T.pack (show status), decodeUtf8 message])
