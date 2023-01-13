module OpenAI.API.V1.Completion.Client where
import Data.Text (Text, pack)
import OpenAI.API.V1.Configuration.Configuration ( Configuration (..) )
import OpenAI.API.V1.Completion.Request ( Request )
import OpenAI.API.V1.Completion.Response ( Response(Response) )
import OpenAI.API.V1.Completion.Choice ( Choice(Choice) )
import OpenAI.API.V1.Completion.Usage ( Usage(Usage) )
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust)
-- | OpenAI API description
about :: Text
about = pack "Given a prompt, the model will return one or more predicted \
         \completions, and can also return the probabilities of alter \
         \native tokens at each position."


completion :: Configuration -> Request -> Either Text Response
completion config request = Right  createEmptyResponse 


createEmptyResponse :: Response
createEmptyResponse = 
  Response (pack "") (pack "") 0 (pack "") [createEmptyChoice] createEmptyUsage

-- | Create a new 'Choice' value with default test values
createEmptyChoice :: Choice
createEmptyChoice = Choice (pack "") 0 Nothing (pack "")

-- | Create a new 'Usage' value with default test values
createEmptyUsage :: Usage
createEmptyUsage = Usage 0 0 0