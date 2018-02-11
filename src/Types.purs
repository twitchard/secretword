module Types where

import Prelude
import Data.Maybe (Maybe)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)

type Session = Maybe SessionRec
type SessionRec =
  { secretWord :: String
  , guesses :: Array String
  , status :: Status
  }
data Status = Normal | GivingUp | Loading
instance wfStatus :: WriteForeign Status where
  writeImpl Normal = write "normal"
  writeImpl GivingUp = write "GivingUp"
  writeImpl Loading = write "Loading"

instance rfStatus :: ReadForeign Status where
  readImpl f = read f <#> readStatus where
    readStatus s
      | s == "GivingUp" = GivingUp
      | s == "Loading" = Loading
      | otherwise = Normal

data Error
  = RequestParseError
  | IntentParseError
  | SlotParseError

data Input
  = Launch
  | Yes
  | No
  | Help
  | Stop
  | Cancel
  | Guess String
  | GiveUp
  | Thinking
  | SessionEnded
  | ErrorInput Error

data Output
  = JustCard Card
  | JustSpeech
      { speech :: Speech
      , reprompt :: Maybe Speech
      }
  | SpeechAndCard
      { speech :: Speech
      , reprompt :: Maybe Speech
      , card :: Card
      }

type Card =
  { type :: String
  , title :: String
  , content :: String
  }

data Speech
  = Text String
  | SSML String

type Response =
  { session :: Session
  , output :: Output
  }
