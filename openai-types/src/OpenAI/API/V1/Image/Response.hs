{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Image.Response where
import GHC.Generics (Generic)
import OpenAI.API.V1.Image.ImageData (ImageData)
import Data.Aeson (ToJSON (toEncoding), Encoding, pairs, KeyValue ((.=)), FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types
    ( FromJSON(parseJSON),
      Value(Object),
      Parser,
      typeMismatch,
      ToJSON(toEncoding),
      Encoding,
      KeyValue((.=)),
      pairs,
      (.:) )

data Response = Response {
    created :: Integer,
    reponseData :: [ImageData]
} deriving (Show, Eq, Generic)

imageResponse :: Response
imageResponse = Response {
    created = 0,
    reponseData = []
}

instance ToJSON Response where
    toEncoding Response {..} = pairs $ mconcat
        [ "created" .= created
        , "data" .= reponseData
        ]

instance FromJSON Response where
    parseJSON (Object o) = do
        created <- o .: "created"
        reponseData <- o .: "data"
        return $ Response {..}
    parseJSON invalid = typeMismatch "Response" invalid
