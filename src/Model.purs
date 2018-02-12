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
          , "How about {Word}"
          ]
        }
      , { name: "SpellingIntent"
        , slots: 
          [ { "name" : "FirstLetter"
            , "type": "Letter"
            }
          , { "name" : "SecondLetter"
            , "type": "Letter"
            }
          , { "name" : "ThirdLetter"
            , "type": "Letter"
            }
          , { "name" : "FourthLetter"
            , "type": "Letter"
            }
          , { "name" : "FifthLetter"
            , "type": "Letter"
            }
          ]
        , samples: 
          [ "I guess {FirstLetter} {SecondLetter} {ThirdLetter} {FourthLetter} {FifthLetter}"
          , "{FirstLetter} {SecondLetter} {ThirdLetter} {FourthLetter} {FifthLetter}"
          , "How about {FirstLetter} {SecondLetter} {ThirdLetter} {FourthLetter} {FifthLetter}"
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
      , letterType
      ]
  }
  where
  fiveLetterWordType = { name : "FiveLetterWord"
                       , values : map toValue commonFiveLetterWords
                       }
  letterType = { name : "Letter"
               , values : map toValue (alphabet <> natoAlphabet)
               }

  toValue word = 
    { name :
        { value : word
        , synonyms : []
        }
    }

  natoAlphabet =
    [ "Alpha"
    , "Bravo"
    , "Charlie"
    , "Delta"
    , "Echo"
    , "Foxtrot"
    , "Golf"
    , "Hotel"
    , "India"
    , "Juliet"
    , "Kilo"
    , "Lima"
    , "Mike"
    , "November"
    , "Oscar"
    , "Papa"
    , "Quebec"
    , "Romeo"
    , "Sierra"
    , "Tango"
    , "Uniform"
    , "Victor"
    , "Whiskey"
    , "X-ray"
    , "Yankee"
    , "Zulu"
    ]

  alphabet =
    [ "A"
    , "B"
    , "C"
    , "D"
    , "E"
    , "F"
    , "G"
    , "H"
    , "I"
    , "J"
    , "K"
    , "L"
    , "M"
    , "N"
    , "O"
    , "P"
    , "Q"
    , "R"
    , "S"
    , "T"
    , "U"
    , "V"
    , "W"
    , "X"
    , "Y"
    , "Z"
    ]

