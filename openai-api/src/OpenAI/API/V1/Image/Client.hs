{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Image.Client where
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Common.Const as Const
import OpenAI.API.V1.Image.Request (Request)
import OpenAI.API.V1.Image.Response (Response)
import OpenAI.API.V1.Common.Configuration
    ( fromEnvVariables, getHeaders )
import Data.Aeson (encode)
import Network.Wreq (postWith, asJSON, responseStatus, responseBody, statusCode, statusMessage, partFile, partText, partBS)
import Control.Lens ((^.))
import qualified OpenAI.API.V1.Image.Edit.Request as IER
import qualified Data.ByteString.Lazy as BL
import qualified OpenAI.API.V1.Util.FileUtil as FU
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as LB


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
        requestBody = encode request
        finalUrl = url <> "generations"
    rsp <- asJSON =<< postWith headers finalUrl requestBody
    let status = rsp ^. responseStatus . statusCode
        response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)

createImageEdit :: IER.Request -> IO(Either Text Response)
createImageEdit request = do
    -- Leer archivos image y mask
    let imagePath = IER.image request
        maskPath = fromMaybe "" (IER.mask request)
    -- Leer archivos image y mask
    image <- LB.readFile imagePath
    --mask <- readFile maskPath
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
        requestBody = [ partFile "image"  imagePath
                , partText "prompt" (IER.prompt request)
                , partText "n" $ T.pack . show $ fromMaybe 0 (IER.n request)
                , partText "size" $ fromMaybe "" (IER.size request)
                ]
        finalUrl = url <> "edits"
    print "Before calling..."
    rsp <- asJSON =<< postWith headers finalUrl requestBody
    print rsp
    let status = rsp ^. responseStatus . statusCode
        response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)