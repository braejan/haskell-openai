{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Common.Configuration where
import OpenAI.API.V1.Configuration.Configuration (Configuration (..), createEmptyConfiguration)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)


fromEnvVariables :: IO Configuration
fromEnvVariables = do
    apiKeyValue <- lookupEnv "OPEN_AI_API_KEY"
    orgValue <- lookupEnv "OPEN_AI_API_ORGANIZATION"
    pure $ createEmptyConfiguration {
        apiKey = T.pack $ fromMaybe "" apiKeyValue,
        organization = T.pack $ fromMaybe "" orgValue
    }


createConfiguration :: Text -> Text -> Configuration
createConfiguration apiKeyValue orgValue = createEmptyConfiguration {
    apiKey = apiKeyValue,
    organization = orgValue
}