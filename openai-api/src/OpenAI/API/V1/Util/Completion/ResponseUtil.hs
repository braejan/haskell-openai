module OpenAI.API.V1.Util.Completion.ResponseUtil where
import OpenAI.API.V1.Completion.Response (Response (choices))
import OpenAI.API.V1.Completion.Choice (Choice)


getChoicesFromResponse :: Response -> [Choice]
getChoicesFromResponse = choices