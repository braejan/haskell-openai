{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Model.Model where
import Data.Text (Text)
import OpenAI.API.V1.Model.Permission (Permission)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), pairs, KeyValue ((.=)), Encoding, FromJSON (parseJSON), Value (Object), (.:), (.:?))
import Data.Aeson.Types
    ( KeyValue((.=)),
      ToJSON(toEncoding),
      FromJSON(parseJSON),
      Value(Object),
      Parser,
      typeMismatch,
      Encoding,
      pairs,
      (.:),
      (.:?) )


{- | Data type representing a model object
-}
data Model = Model {
    id :: Text,
    -- ^ The ID of the model
    object :: Text,
    -- ^ The type of the object
    created :: Int,
    -- ^ The timestamp of when the model object was created
    ownedBy :: Text,
    -- ^ The owner of the model
    permission :: [Permission],
    -- ^ The permissions of the model
    root :: Text,
    -- ^ The root model of the model
    parent :: Maybe Text
    -- ^ The parent model of the model
} deriving (Eq, Show, Generic)

createModel :: Model
createModel = Model {
    id = "",
    object = "",
    created = 0,
    ownedBy = "",
    permission = [],
    root = "",
    parent = Nothing
}

instance ToJSON Model where
  toEncoding :: Model -> Encoding
  toEncoding Model {..} = pairs $ mconcat
    [ "id" .= id
    , "object" .= object
    , "created" .= created
    , "owned_by" .= ownedBy
    , "permission" .= permission
    , "root" .= root
    , "parent" .= parent
    ]

instance FromJSON Model where
  parseJSON :: Value -> Parser Model
  parseJSON (Object o) = do
    id <- o .: "id"
    object <- o .: "object"
    created <- o .: "created"
    ownedBy <- o .: "owned_by"
    permission <- o .: "permission"
    root <- o .: "root"
    parent <- o .:? "parent"
    return $ Model{..}
  parseJSON invalid = typeMismatch "Model" invalid
