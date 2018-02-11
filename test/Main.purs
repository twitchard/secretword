module Test.Main where

import Prelude

import DB (class DB)
import Data.Foreign (Foreign)
import Data.Lens (_Just, view)
import Data.Maybe (Maybe(..), isNothing)
import Simple.JSON (write)
import Skill (handle, speeches)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert) as Assert
import Test.Unit.Main (runTest)
import Types (Session, textOf)
import Web.Amazon.Alexa.Lens (_outputSpeech, _response, _sessionAttributes, _text, _type)

dummy :: Foreign
dummy = write {}

newtype TestDB = TestDB
  { sess :: Maybe Session }

instance testDB :: DB TestDB (|e) where
  saveSession db _ _ = pure unit
  loadSession (TestDB db) _ = pure db.sess
  eraseSession db _ = pure unit

emptyDB :: TestDB
emptyDB = TestDB {sess : Nothing}

dbWith :: Session → TestDB
dbWith sess = TestDB { sess : Just sess }

testEvent = {
  "version": "1.0",
  "session": {
    "new": true,
    "sessionId": "sessionId",
    "application": { "applicationId": "applicationId" },
    "user": { "userId": "userId" },
    "attributes": {
      "secretWord": "blush",
      "guesses": ([] :: Array String),
      "status": "Normal"
    }
  },
  "context": {
    "AudioPlayer": { "playerActivity": "IDLE" },
    "Display": { "token": "" },
    "System": {
      "application": { "applicationId": "applicationId" },
      "user": { "userId": "userId" },
      "device": {
        "deviceId": "deviceId",
        "supportedInterfaces": {
          "AudioPlayer": {},
          "Display": {
            "templateVersion": "1.0",
            "markupVersion": "1.0"
          }
        }
      },
      "apiEndpoint": "apiEndpoint",
      "apiAccessToken": "apiAccessToken"
    }
  },
  "request": {
    "type": "IntentRequest",
    "intent": {
      "name" : "GuessIntent",
      "slots" : {
        "Word" : { "value" : "blush" }
      }
    },
    "requestId": "amzn1.echo-api.request.8d5730f6-0e10-4682-be13-153823b630b9",
    "timestamp": "2018-01-07T23:37:53Z",
    "locale": "en-US"
  }
}

main = runTest do
  suite "End-to-end" do
    test "Handling a guess of the correct answer" do
      let db = emptyDB
          event = write testEvent
          context = dummy

      result <- handle db event context

      let speechType = view (_response <<< _outputSpeech <<< _Just <<< _type) result
          speechText = view (_response <<< _outputSpeech <<< _Just <<< _text ) result
          session = view (_sessionAttributes ) result

      Assert.assert
        (speechType <> " should be PlainText")
        $ speechType == "PlainText"

      Assert.assert
        ("Alexa should say \"" <> (textOf $ speeches.youWin 1) <> "\", not \"" <> speechText<> "\"")
        $ speechText == (textOf $ speeches.youWin 1)

      Assert.assert
        ("The session should end")
        $ isNothing session

    test "Handling a malformed AlexaRequest" do
      let db = emptyDB
          event = write
                    (testEvent
                      { request = testEvent.request
                                    { type = "MalformedRequest"
                                    }
                      }
                    )
          context = dummy

      result <- handle db event context

      let speechType = view (_response <<< _outputSpeech <<< _Just <<< _type) result
          speechText = view (_response <<< _outputSpeech <<< _Just <<< _text ) result
          session = view (_sessionAttributes ) result

      Assert.assert
        (speechType <> " should be PlainText")
        $ speechType == "PlainText"

      Assert.assert
        ("Alexa should say \"" <> (textOf speeches.couldntUnderstand) <> "\", not \"" <> speechText <> "\"")
        $ speechText == (textOf speeches.couldntUnderstand)

      Assert.assert
        ("The session should end")
        $ isNothing session

