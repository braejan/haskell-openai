# Haskell OpenAI ChatGPT3

This project provides an implementation of a chatbot using the OpenAI GPT-3 model. The main module for interacting with the chatbot can be found at `OpenAI.API.V1.Chat.ChatGPT3.Chat` and can be used via the `chatBot` function. Remember it's necessary to set up the OpenAI variable `OPENAI_API_KEY` previously to run it.

To install this project, run the following command in your terminal:

```bash
cabal install exe:openai-labs
```
Execute the final binary and it will open a welcome screen like the next one:
```bash
  _   _   _   _   _   _   _     _   _     _   _   _   _   _   _
 / \ / \ / \ / \ / \ / \ / \   / \ / \   / \ / \ / \ / \ / \ / \
( P | o | w | e | r | e | d ) ( b | y ) ( O | p | e | n | A | I )
 \_/ \_/ \_/ \_/ \_/ \_/ \_/   \_/ \_/   \_/ \_/ \_/ \_/ \_/ \_/

🚀 Starting...
🤖🤖🤖🤖🤖 Welcome! It's great to have you here. Can you tell me a bit about yourself so I can better understand your needs? What are you looking for help with today?
📝📝📝📝📝
#Start to write here
```

`Note:` An OpenAI API key is required to use this chatbot.