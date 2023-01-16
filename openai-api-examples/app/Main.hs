{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text (Text)
import qualified Data.Text as T
import OpenAI.API.V1.Completion.Response ( Response (..) )
import System.Environment (lookupEnv)
import OpenAI.API.V1.Configuration.Configuration (Configuration(..))
import OpenAI.API.V1.Completion.Request (createEmptyRequest, Request (..))
import qualified OpenAI.API.V1.Completion.Client as Client
import qualified OpenAI.API.V1.Completion.Request as Request
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import OpenAI.API.V1.Common.Configuration (fromEnvVariables)
import Control.Monad (forM_)
import OpenAI.API.V1.Completion.Choice (Choice(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import OpenAI.API.V1.Completion.Client (fastCompletion)
import Data.Aeson (ToJSON(toJSON), decode, encode)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    putStrLn "Calling OpenAI API Completion:"
    completion <- fastCompletion createEmptyRequest { 
        Request.model = "text-davinci-003",
        prompt  = Just $ Left "Say this is a test"
    }
    case completion of
        Left error -> do
            print error
        Right response -> print $ encode response



-- callCompletion :: IO(Either Text Response)
-- callCompletion = do
--     apiKey <- lookupEnv "OPEN_AI_API_KEY"
--     case apiKey of
--         Nothing -> pure $ Left "No OpenAI API key found."
--         Just apiKeyValue -> do
--             organization <- lookupEnv "OPEN_AI_API_ORGANIZATION"
--             case organization of
--                 Nothing -> processWithoutOrg $ T.pack apiKeyValue
--                 Just orgValue -> processWithOrg (T.pack apiKeyValue) (T.pack orgValue)


-- processWithoutOrg :: Text -> IO(Either Text Response)
-- processWithoutOrg apiKeyValue = processWithOrg apiKeyValue ""

-- processWithOrg :: Text -> Text -> IO(Either Text Response)
-- processWithOrg apiKeyValue orgValue = do
--     let configuration = Configuration {
--         apiKey = apiKeyValue,
--         organization = orgValue
--     }
--     let request = createEmptyRequest {
--         Request.model = "text-davinci-003",
--         prompt = Just $ Left "Crear una historia de buenos días",
--         maxTokens = Just 50,
--         temperature = Just 0
--     }
--     Client.completion configuration request


-- chat :: IO()
-- chat = do
--     putStr "User >>> "
--     userInput <- getLine
--     config <- fromEnvVariables
--     let request = createEmptyRequest {
--         Request.model = "text-davinci-003",
--         prompt = Just $ Left (T.pack userInput),
--         maxTokens = Just 50,
--         temperature = Just 0,
--         topP = Just 1,
--         n = Just 1,
--         stop = Just $ Left "\n"
--     }
--     result <- Client.completion config request
--     putStr "Chat >>> "
--     case result of
--         Left msg -> putStrLn $ "I'm responding with error: " <> show msg
--         Right chatResponse -> print chatResponse
--         -- Right chatResponse -> forM_ (choices chatResponse) $ \choice -> do
--         --     liftIO . putStrLn $ T.unpack (text choice)
--     chat


    