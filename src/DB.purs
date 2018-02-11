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

saveSession :: ∀ e. DynamoClient → String → Session → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) Unit
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

loadSession :: ∀ e. DynamoClient → String → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) (Maybe Session)
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

eraseSession :: ∀ e. DynamoClient → String → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) Unit
eraseSession cli id = do
  deleteRecord
    cli
    "secretword__Sessions"
    ( write
      { userId : id }
    )
