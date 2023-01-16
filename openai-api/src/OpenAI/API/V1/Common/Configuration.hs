{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Common.Configuration where
import OpenAI.API.V1.Configuration.Configuration (Configuration (..), createEmptyConfiguration)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Network.Wreq (Options, defaults, header)
import qualified Data.Text.Encoding as TE
import Control.Lens ((&), (.~))
import Control.Monad.Reader (ReaderT(runReaderT))

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

-- | getHeaders function takes in a 'Configuration' data and returns an 'Options' data
-- It sets the header "Content-Type" to "application/json"
-- It sets the header "Authorization" to "Bearer " and the apiKey of the passed configuration
-- It sets the header "OpenAI-Organization" to the organization of the passed configuration
getHeaders :: Configuration -> Options
getHeaders configuration = defaults
                    & header "Content-Type" .~ ["application/json"]
                    & header "Authorization" .~ ["Bearer " <> TE.encodeUtf8 (apiKey configuration)]
                    & header "OpenAI-Organization" .~ [TE.encodeUtf8 (organization configuration)]