
# :construction: Under Development :construction:
# Welcome to the Haskell OpenAI API library!

This library provides a wrapper for the OpenAI API, making it easy to implement services exposed by the OpenAI API in your Haskell projects.

## Clients

### Completion
Creates a completion for the provided prompt and parameters
#### General completion
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified OpenAI.API.V1.Completion.Client as Client
import OpenAI.API.V1.Configuration.Configuration (Configuration(Configuration))
import OpenAI.API.V1.Completion.Request (Request(..), createEmptyRequest)

main :: IO ()
main = do
    --Calling using a custom configuration:
    let configuration = Configuration "YOUR_API_KEY" "YOUR_ORGANIZATION"
    let request = createEmptyRequest {
        model = "your-favorite-model",
        prompt = Just $ Left "my custom prompt"
    --  to use an array you should use:
    --  prompt = Just $ Rigth ["custom prompt 0", "custom prompt n"]
    }
    call <- Client.completion configuration request
    case call of
      Left error -> print ("call with error: " <> error)
      Right response -> print response 
```

Creates a completion for the provided configuration by environment variables. This use the ```OPEN_AI_API_KEY``` and ```OPEN_AI_ORGANIZATION``` variables.
#### Fast completion
```haskell
. . .
call <- Client.fastCompletion request
. . .
```