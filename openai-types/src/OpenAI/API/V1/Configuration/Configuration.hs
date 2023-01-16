{-# LANGUAGE OverloadedStrings #-}
module OpenAI.API.V1.Configuration.Configuration where
import Data.Text

data Configuration = Configuration 
  { apiKey :: Text
   ,organization :: Text
  }


createEmptyConfiguration :: Configuration
createEmptyConfiguration = Configuration {
  apiKey = "",
  organization = ""
}