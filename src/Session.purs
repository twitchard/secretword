module Session where

import Prelude

import AWS.DynamoDB (DYNAMO, DynamoClient, deleteRecord, loadRecord, saveRecord)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read, write, writeJSON)
import Type.Prelude (SProxy(..))

type Session = Maybe
  { secretWord :: String
  , guesses :: Array String
  , status :: Status
  }

_guesses :: ∀ r. Lens' {guesses :: Array String | r } (Array String)
_guesses = prop (SProxy :: SProxy "guesses")

{- status is also a lens
-}
_status :: ∀ r. Lens' {status :: Status | r } Status
_status = prop (SProxy :: SProxy "status")

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
