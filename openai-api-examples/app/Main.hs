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
import OpenAI.API.V1.Edit.Client (fastTextEdit)

main :: IO ()
main = do
    callTextEdit



callTextEdit :: IO()
callTextEdit = do
    result <- fastTextEdit "Fox this text." "Correct text"
    print result
    