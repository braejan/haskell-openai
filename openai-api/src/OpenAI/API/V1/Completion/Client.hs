{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Completion.Client where
import Data.Text (Text)
import OpenAI.API.V1.Configuration.Configuration ( Configuration (..), createEmptyConfiguration )
import OpenAI.API.V1.Completion.Request ( Request (..) )
import OpenAI.API.V1.Completion.Response ( Response(Response, choices))
import qualified OpenAI.API.V1.Common.Const as Const
import Network.Wreq (asJSON, defaults, postWith, Options)
import Control.Lens ((^.), (&), (.~))
import Network.Wreq.Lens (responseBody, header, responseStatus, statusCode, statusMessage)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import OpenAI.API.V1.Common.Configuration (fromEnvVariables, getHeaders)
import Data.Aeson (encode)
import Control.Monad.IO.Class ()
import Control.Monad.Except (runExceptT)
-- | OpenAI API description
about :: Text
about = "Given a prompt, the model will return one or more predicted \
         \completions, and can also return the probabilities of alter \
         \native tokens at each position."

-- | url display the final url for completions.
url :: String
url = T.unpack $ Const.baseURL <> "completions"


-- | createCompletion is a method that takes the configuration from ENV variables and create send a request.
createCompletion :: Request -> IO(Either Text Response)
createCompletion request = do
  configuration <- fromEnvVariables
  let headers = getHeaders configuration
  let requestBody = encode request
  rsp <- asJSON =<< postWith headers url requestBody
  let status = rsp ^. responseStatus . statusCode
  let response = rsp ^. responseBody
  case status of
    200 -> pure $ Right response
    _ -> pure $ Left finalError where
        message = rsp ^. responseStatus . statusMessage
        finalError = T.pack (show status <> ": " <> show message)
