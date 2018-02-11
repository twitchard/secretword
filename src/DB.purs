module DB where

import Prelude

import AWS.DynamoDB (DYNAMO, DynamoClient, saveRecord, loadRecord, deleteRecord)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Simple.JSON (read, write, writeJSON)
import Types (Session, Status(..))

class DB db e | db → e where
  saveSession :: db → String → Session → Aff e Unit
  loadSession :: db → String → Aff e (Maybe Session)
  eraseSession :: db → String → Aff e Unit

instance dynamoClientDB :: DB DynamoClient (console :: CONSOLE, dynamo :: DYNAMO | e) where
  saveSession cli userId sess = do
    case sess of
      Nothing → pure unit
      Just rec → do
        log $ "Saving session: " <> (writeJSON
                                      { userId : userId
                                      , guesses: rec.guesses
                                      , secretWord : rec.secretWord
                                      , status : Normal
                                      }
                                    )
        saveRecord
          cli
          "secretword__Sessions"
          ( write
            { userId : userId
            , guesses: rec.guesses
            , secretWord : rec.secretWord
            , status : "normal"
            }
          )

  loadSession cli id = do
    rec <- loadRecord
      cli
      "secretword__Sessions"
      ( write
        { userId : id }
      )
    log ("the session was " <> (writeJSON rec))
    case runExcept (read rec) of
      Left _ → pure Nothing
      Right (result :: {"Item" :: Session})→ pure $ Just (result."Item")

  eraseSession cli id = do
    deleteRecord
      cli
      "secretword__Sessions"
      ( write
        { userId : id }
      )
