module Main 
  ( main
  , handler
  )
  where

import Prelude

import AWS.DynamoDB (DYNAMO)
import Amazon.Alexa.Skill.LanguageModel (LanguageModel)
import Amazon.Alexa.Skill.Manifest (Manifest)
import Control.Monad.Aff (Fiber)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3)
import Data.Foreign (Foreign)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.StrMap (empty, singleton)
import Handler (myHandler)
import SecretWord.Words (commonFiveLetterWords)
import Simple.JSON (writeJSON)
import Web.AWS.Lambda (makeHandler)

foreign import args ::
  { bin :: String
  , command :: String
  }

type Effects a =
  ( dynamo :: DYNAMO
  , console :: CONSOLE
  , random :: RANDOM
  | a
  )

handler :: forall t10.
   EffFn3
     ( Effects t10 )
     Foreign
     Foreign
     (EffFn2 ( Effects t10 ) (Nullable Error) Foreign Unit)
     (Fiber ( Effects t10 ) Unit)
handler = makeHandler myHandler

main :: forall t31.
   Eff
     ( console :: CONSOLE
     | t31
     )
     Unit
main = runCommand
  where
    runCommand
      | args.command == "manifest" = generateManifest
      | args.command == "model" = generateModel
      | otherwise = printUsage

    generateManifest = log $ writeJSON manifest
    generateModel    = log $ writeJSON model
    printUsage       = log $ "TODO"

manifest :: Manifest
manifest =
  { skillManifest :
      { publishingInformation :
          { locales :
              singleton "en-US"
                { name : "secret word"
                , summary : "Guess the secret word"
                , examplePhrases :
                    [ "Alexa, open Secret word"
                    , "I guess \"track\"."
                    , "I give up"
                    ]
                , description : "Figure out my secret five-letter word in as few guesses as possible. Each time you guess, I will say how many letters your guess shares with the secret word."
                , keywords :
                    [ "game"
                    , "words"
                    , "puzzle"
                    ]
                }
          , isAvailableWorldWide : true
          , testingInstructions : ""
          , category : "GAMES"
          , distributionCountries : []
          }
      , apis : 
          { "custom" :
              { "endpoint":
                  { "sourceDir" : NullOrUndefined $ Just "lambda/custom"
                  , "uri" : NullOrUndefined $ Nothing
                  }
              }
          }
      , manifestVersion : "1.0"
      , permissions : []
      , privacyAndCompliance :
          { allowsPurchases : false
          , usesPersonalInfo : false
          , isChildDirected : false
          , isExportCompliant : true
          , containsAds : false
          , locales : empty
          }
      , events : NullOrUndefined Nothing
      , subscriptions : []
      , regions : empty
      }
  }
model ::
  { interactionModel ::
    { languageModel :: LanguageModel }
  }

model =
  { interactionModel :
    { languageModel : americanEnglish }
  }


americanEnglish :: LanguageModel
americanEnglish =
  { invocationName: "secret word"
  , intents:
      [ { name: "AMAZON.CancelIntent"
        , samples: []
        , slots : []
        }
      , { name: "AMAZON.HelpIntent"
        , samples: []
        , slots : []
        }
      , { name: "AMAZON.StopIntent"
        , samples: []
        , slots : []
        }
      , { name: "GiveUpIntent"
        , samples: [ "I give up" ]
        , slots : []
        }
      , { name: "AMAZON.YesIntent"
        , samples: []
        , slots : []
        }
      , { name: "AMAZON.NoIntent"
        , samples: []
        , slots : []
        }
      , { name: "GuessIntent"
        , slots:
          [ { "name": "Word"
            , "type": "FiveLetterWord"
            }
          ]
        , samples:
          [ "Guess {Word}"
          , "I guess {Word}"
          , "My guess is {Word}"
          ]
        }
      , { name: "ThinkingIntent"
        , slots : []
        , samples:
          [ "I'm thinking"
          , "Still thinking"
          ]
        }
      ]
  , types:
      [ fiveLetterWordType
      ]
  }
  where
  fiveLetterWordType = { name : "FiveLetterWord"
                       , values : map toValue commonFiveLetterWords
                       }
  toValue word = 
    { name :
        { value : word
        , synonyms : []
        }
    }
