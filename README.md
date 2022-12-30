# Welcome to the Haskell OpenAI API library!

This library provides a wrapper for the OpenAI API, making it easy to implement services exposed by the OpenAI API in your Haskell projects. With this library, you can perform basic query operations on the OpenAI API with just a few lines of code.

## Installation

To install this library, you can use [Stack](https://docs.haskellstack.org/en/stable/README/):

```bash
stack install haskell-gpt3
```

## Usage

To use this library, you will need an OpenAI API key. You can sign up for a free API key at the OpenAI website.
Once you have your API key, you can use the library like this:

```haskell
import qualified OpenAI

-- Your OpenAI API key
apiKey :: Text
apiKey = "Your API key here"

main :: IO ()
main = do
  -- Initialize the OpenAI client with your API key
  client <- OpenAI.newClient apiKey

  -- Make a request to the OpenAI API
  response <- OpenAI.request client (OpenAI.Completion "Write something here")

  -- Process the response from the API
  case response of
    Right result ->
      -- Do something with the result
      putStrLn $ "Response from the API: " ++ show result
    Left error ->
      -- Handle the error
      putStrLn $ "Error making the request: " ++ show error
```

[Last analyst](https://lift.sonatype.com/api/badge/github.com/braejan/haskell-gpt3)
