# Secret Word

*Secret Word* is an Alexa Skill, based off the logic-oriented word game ["Jotto"](https://en.wikipedia.org/wiki/Jotto), implemented in Purescript.

## Rules

When the game starts, a five-letter "secret word" is selected at random from a [corpus of common English words](src/Words.purs). The player's goal is to discover the secret word in as few turns as possible. Each turn, the player guesses a five-letter English word, and is told the total number of letters that the guess has in common with the secret word.

### Duplicate letters
Letters that are duplicated in both the player's guess and the secret word count multiple times towards the total, once for each duplication that is present in both words. The following table is informative.

| Word      | Word          | Total |
|-----------|---------------|-------|
| ru**mm**y | **m**ada**m** |   2   |
| a**pp**le | **p**roud     |   1   |
| ch**eer** | **eer**i*e*   |   3   |

Here, *rummy* and *madam* are reported to have 2 letters in common, because they both contain 2 m's. The words *apple* and *proud* are reported to have only 1 letter in common, because although p appears twice in apple, it only appears once in proud. Finally, the words *cheer* and *eerie* are reported to have 3 letters in common. Eerie has three e's, but cheer has only two, so e contributes 2 to the total. Both words also have an *r*, so the reported total is 3.

## Implementation

Almost the entire implementation for Secret Word is contained in a single file, the [Main.purs](src/Main.purs), which defines a handler function intended executed on AWS Lambda. Right now that file contains a lot of comments (maybe too many) that might be an interesting introduction to how Alexa Skills work given in the context of a very specific example.

In the course of building this skill I wrote two other "libraries". One is [purescript-alexa](https://github.com/twitchard/purescript-alexa). Right now that repository is little more than (woefully incomplete) type definitions for 'AlexaRequest' and 'AlexaResponse', which describe (respectively) the format of the inputs provided into an Alexa skill and the format of the expected output of the skill. I'd like to make it a stronger API however, providing more guarantees about correctness via types, and including some of the helper functions I ended up writing in [Main.purs](src/Main.purs). My even greater ambitions are to be able to generate the JSON of the [interaction model](models/en-US.json) from the same type definitions I use to guide my implementation of the handlers of those "intents" inside my skill.

The other 'library' I wrote is [purescript-lambda-handler](https://github.com/twitchard/purescript-aws-lambda-handler) which is a single function, useful for appropriately uncurrying a function in AWS lambda land. Probably doesn't deserve to be on its own in a repo.

## Deployment

Secret word is deployed as an AWS Lambda function.

Amazons [Alexa Skills Kit CLI (ask-cli)](https://www.npmjs.com/package/ask-cli) makes deploying an Alexa skill pretty simple. I strongly recommend you use it if you have any desire to build a skill of your own.
.
```
npm install -g ask-cli
```
)

1. The default directory structure produced by ask-cli (`ask clone`), at least by default, expects the compiled javascript to live underneath the `lambda/custom` subdirectory of the repo. Unfortunately purs ide / my editor plugin have expressed a strong preference that my build reside in `output` right next to `src`. Thus, in my `package.json` I've defined an npm task
`npm run deploy` that first builds my skill into `lambda/custom/output` and then runs `ask deploy -t lambda`.

2. By default, the AWS lambda function will try to hook into `exports.handler` inside `index.js`. I went in to the AWS Console and manually changed the handler of my lambda function to `output/Main/index.handler`. Make sure your directory names don't contain any periods (lots of purescript directories do) because AWS lambda won't like that. (Thanks to [Louis Pilford, author of purescript-aws-lambda-express](https://pursuit.purescript.org/packages/purescript-aws-lambda-express/1.0.2) for this warning).

3. I also ended up manually setting permissions for my Lambda function to access DynamoDB.
