module OpenAI.API.V1.Util.FileUtil where

import System.IO ()
import Control.Exception ( try )

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path = try (readFile path) >>= handle
    where handle :: Either IOError String -> IO (Maybe String)
          handle (Right contents) = return (Just contents)
          handle (Left _) = return Nothing
