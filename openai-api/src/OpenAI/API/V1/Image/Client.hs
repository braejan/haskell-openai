{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Image.Client where
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAI.API.V1.Common.Const as Const
import OpenAI.API.V1.Image.Request (Request)
import OpenAI.API.V1.Image.Response (Response)
import OpenAI.API.V1.Common.Configuration
    ( fromEnvVariables, getHeaders )
import Data.Aeson (encode, object)
import Network.Wreq (postWith, asJSON, responseStatus, responseBody, statusCode, statusMessage, partFile, partText, partBS)
import Control.Lens ((^.))
import qualified OpenAI.API.V1.Image.Edit.Request as IER
import qualified OpenAI.API.V1.Image.Variation.Request as IVR
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust)
import qualified Data.ByteString as LB


about :: Text
about = "Given a prompt and/or an input image, the model will generate a new image."

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
-- | createImageEdit Creates an edited or extended image given an original image and a prompt.
createImageEdit :: IER.Request -> IO(Either Text Response)
createImageEdit request = do
    let imagePath = IER.image request
        maskPath =  IER.mask request
        n = case IER.n request of
                Nothing -> Nothing
                Just value -> Just $ T.pack $ show value
    image <- LB.readFile imagePath
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
        requestBody =
                [ partFile "image"  imagePath
                , partText "prompt" (IER.prompt request)
                ] <>
                [partFile "mask" (fromMaybe "" maskPath) | isJust maskPath] <>
                [partText "size" (fromMaybe "" (IER.size request)) | isJust (IER.size request)] <>
                [partText "n" (fromMaybe "" n) | isJust n] <>
                [partText "response_format" (fromMaybe "" (IER.responseFormat request)) | isJust (IER.responseFormat request)] <>
                [partText "user" (fromMaybe "" (IER.user request)) | isJust (IER.user request)]
        finalUrl = url <> "edits"
    rsp <- asJSON =<< postWith headers finalUrl requestBody
    let status = rsp ^. responseStatus . statusCode
        response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)

-- | createImageVariation Creates a variation of a given image.
createImageVariation :: IVR.Request -> IO (Either Text Response)
createImageVariation request = do
    let imagePath = IVR.image request
        n = case IVR.n request of
                Nothing -> Nothing
                Just value -> Just $ T.pack $ show value
    image <- LB.readFile imagePath
    configuration <- fromEnvVariables
    let headers = getHeaders configuration
        requestBody =
                [partFile "image"  imagePath] <>
                [partText "size" (fromMaybe "" (IVR.size request)) | isJust (IVR.size request)] <>
                [partText "n" (fromMaybe "" n) | isJust n] <>
                [partText "response_format" (fromMaybe "" (IVR.responseFormat request)) | isJust (IVR.responseFormat request)] <>
                [partText "user" (fromMaybe "" (IVR.user request)) | isJust (IVR.user request)]
        finalUrl = url <> "variations"
    rsp <- asJSON =<< postWith headers finalUrl requestBody
    let status = rsp ^. responseStatus . statusCode
        response = rsp ^. responseBody
    case status of
        200 -> pure $ Right response
        _ -> pure $ Left finalError where
            message = rsp ^. responseStatus . statusMessage
            finalError = T.pack (show status <> ": " <> show message)
