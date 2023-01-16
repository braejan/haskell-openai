{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Common.Const 
  (
    baseURL
  ) where

import Data.Text (Text)

baseURL :: Text
baseURL = "https://api.openai.com/v1/"
