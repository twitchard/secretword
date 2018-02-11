module Test.Main
  ( main )
  where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import DB (class DB)
import Data.Foreign (Foreign)
import Data.Lens (_Just, view)
import Data.Maybe (Maybe(Nothing, Just))
import Simple.JSON (write, writeJSON)
import Skill (handle, speeches)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert) as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Types (Session, textOf)
import Web.Amazon.Alexa.Lens (_outputSpeech, _response, _sessionAttributes, _text, _type)

dummy :: Foreign
dummy = write {}

newtype TestDB = TestDB
  { sess :: Maybe Session }

instance testDB :: DB TestDB (|e) where
  saveSession db _ _ = pure unit
    $ text == expectedText

  Assert.assert
    ("The session should be \"" <> writeJSON expectedSession <> "\", not \"" <> writeJSON session <> "\".")
    $ eqSessions expectedSession session

  where
    eqSessions Nothing Nothing = true
    eqSessions (Just s) (Just s') =
      s.secretWord == s'.secretWord
      && s.guesses == s'.guesses
      && s.status == s'.status
    eqSessions _ _ = false

main :: forall t255.
   Eff
     ( console :: CONSOLE
     , testOutput :: TESTOUTPUT
     , avar :: AVAR
     , random :: RANDOM
     | t255
     )
     Unit
main = runTest do
  suite "End-to-end" do
    test "Handling a guess of the correct answer" do
      let event = write testEvent
          db = emptyDB
          expectedType = "PlainText"
          expectedText = textOf $ speeches.youWin 1
          expectedSession = Nothing

      testHandle event db expectedType expectedText expectedSession

    test "Handling a malformed AlexaRequest" do
      let event = write
                    (testEvent
                      { request = testEvent.request
                                    { type = "MalformedRequest"
                                    }
                      }
                    )

          db = emptyDB
          expectedType = "PlainText"
          expectedText = textOf $ speeches.couldntUnderstand
          expectedSession = Nothing
      testHandle event db expectedType expectedText expectedSession

    test "Handling a unknown intent" do
      let event = write
                    (testEvent
                      { request = testEvent.request
                        { intent = testEvent.request.intent
                          { name = "Unknown" }
                        }
                      , session = testEvent.session
                        { attributes = (Nothing :: Maybe String) }
                      }
                    )

          db = emptyDB
          expectedType = "PlainText"
          expectedText = textOf $ speeches.couldntUnderstand
          expectedSession = Nothing
      testHandle event db expectedType expectedText expectedSession
  where
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
    
