module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write)

type Session = Maybe SessionRec
type SessionRec =
  { secretWord :: String
  , guesses :: Array String
  , status :: Status
  }

data Status = Normal | GivingUp | Loading
derive instance genericStatus :: Generic Status _
instance eqStatus :: Eq Status where
  eq = genericEq
instance showStatus :: Show Status where
  show = genericShow

instance wfStatus :: WriteForeign Status where
  writeImpl Normal = write "Normal"
  writeImpl GivingUp = write "GivingUp"
  writeImpl Loading = write "Loading"

instance rfStatus :: ReadForeign Status where
  readImpl f = read f <#> readStatus where
    readStatus s
      | s == "GivingUp" = GivingUp
      | s == "Loading" = Loading
      | otherwise = Normal

data SkillError
  = RequestParseError
  | IntentParseError
  | SlotParseError

derive instance genericSkillError :: Generic SkillError _
instance eqSkillError :: Eq SkillError where
  eq = genericEq
instance showSkillError :: Show SkillError where
  show = genericShow

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
  | ErrorInput SkillError

derive instance genericInput :: Generic Input _
instance eqInput :: Eq Input where
  eq = genericEq
instance showInput :: Show Input where
  show = genericShow

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

derive instance genericOutput :: Generic Output _
instance showOutput :: Show Output where
  show = genericShow
instance eqOutput :: Eq Output where
  eq = genericEq

newtype Card = Card
  { type :: CardType
  , title :: String
  , content :: String
  }

data CardType = Simple | Standard | LinkAccount
derive instance genericCardType :: Generic CardType _
instance showCardType :: Show CardType where
  show = genericShow
instance eqCardType :: Eq CardType where
  eq = genericEq
instance wfCardType :: WriteForeign CardType where
  writeImpl Simple = write "Simple"
  writeImpl Standard = write "Standard"
  writeImpl LinkAccount = write "LinkAccount"

derive instance newtypeCard :: Newtype Card _
derive instance genericCard :: Generic Card _
instance showCard :: Show Card where
  show = genericShow
instance eqCard :: Eq Card where
  eq = genericEq

data Speech
  = Text String
  | SSML String

textOf :: Speech → String
textOf (Text s) = s
textOf (SSML s) = s

derive instance genericSpeech :: Generic Speech _
instance eqSpeech :: Eq Speech where
  eq = genericEq
instance showSpeech :: Show Speech where
  show = genericShow

type Response =
  { session :: Session
  , output :: Output
  }
