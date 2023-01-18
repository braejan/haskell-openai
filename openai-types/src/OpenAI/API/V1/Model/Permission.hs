{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module OpenAI.API.V1.Model.Permission where
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Aeson (ToJSON (toEncoding), FromJSON (parseJSON), Value (Object), pairs, KeyValue ((.=)), (.:))
import Data.Aeson.Types
    ( KeyValue((.=)),
      ToJSON(toEncoding),
      FromJSON(parseJSON),
      Value(Object),
      Parser,
      typeMismatch,
      Encoding,
      pairs,
      (.:) )

{- | Data type representing a permission object
-}
data Permission = Permission {
    id :: Text,
    -- ^ The ID of the permission object
    object :: Text,
    -- ^ The type of the object
    created :: Int,
    -- ^ The timestamp of when the permission object was created
    allowCreateEngine :: Bool,
    -- ^ Whether or not the permission allows creating engines
    allowSampling :: Bool,
    -- ^ Whether or not the permission allows sampling
    allowLogprobs :: Bool,
    -- ^ Whether or not the permission allows getting log probabilities
    allowSearchIndices :: Bool,
    -- ^ Whether or not the permission allows searching indices
    allowView :: Bool,
    -- ^ Whether or not the permission allows viewing the model
    allowFineTuning :: Bool,
    -- ^ Whether or not the permission allows fine tuning the model
    organization :: Text,
    -- ^ The organization that the permission applies to
    group :: Maybe Text,
    -- ^ The group that the permission applies to, if any
    isBlocking :: Bool
    -- ^ Whether or not the permission is blocking
} deriving (Show, Eq, Generic)

modelPermission :: Permission
modelPermission = Permission {
    id = "",
    object = "",
    created = 0,
    allowCreateEngine = False,
    allowSampling = False,
    allowLogprobs = False,
    allowSearchIndices = False,
    allowView = False,
    allowFineTuning = False,
    organization = "",
    group = Nothing,
    isBlocking = False
}

instance ToJSON Permission where
  toEncoding :: Permission -> Encoding
  toEncoding Permission {..} = pairs $ mconcat
    [ "id" .= id
    , "object" .= object
    , "created" .= created
    , "allow_create_engine" .= allowCreateEngine 
    , "allow_sampling" .= allowSampling 
    , "allow_logprobs" .= allowLogprobs 
    , "allow_search_indices" .= allowSearchIndices 
    , "allow_view" .= allowView 
    , "allow_fine_tuning" .= allowFineTuning 
    , "organization" .= organization 
    , "group" .= group 
    , "is_blocking" .= isBlocking 
    ]

instance FromJSON Permission where
  parseJSON :: Value -> Parser Permission
  parseJSON (Object o) = do
    id <- o .: "id"
    object <- o .: "object"
    created <- o .: "created"
    allowCreateEngine <- o .: "allow_create_engine"
    allowSampling <- o .: "allow_sampling"
    allowLogprobs <- o .: "allow_logprobs"
    allowSearchIndices <- o .: "allow_search_indices"
    allowView <- o .: "allow_view"
    allowFineTuning <- o .: "allow_fine_tuning"
    organization <- o .: "organization"
    group <- o .: "group"
    isBlocking <- o .: "is_blocking"
    return $ Permission{..}
  parseJSON invalid = typeMismatch "Permission" invalid