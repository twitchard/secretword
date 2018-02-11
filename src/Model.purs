module Module where

import Prelude

import Amazon.Alexa.Skill.LanguageModel (LanguageModel)
import SecretWord.Words (commonFiveLetterWords)

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
