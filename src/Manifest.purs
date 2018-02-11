module Manifest where

import Prelude

import Amazon.Alexa.Skill.Manifest (Manifest)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, singleton)

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
