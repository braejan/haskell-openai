module OpenAI.API.V1.Common.Helper where
import Data.Aeson (KeyValue ((.=)), ToJSON, Key)


-- | Create a 'KeyValue' pair from a 'Maybe' value, using 'mempty' if the value is 'Nothing'
--
-- This function is used to create 'KeyValue' pairs for optional fields.
maybeEmpty :: (Monoid b, KeyValue b, ToJSON v) => Key -> Maybe v -> b
maybeEmpty key = maybe mempty (key .=)