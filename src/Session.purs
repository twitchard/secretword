module Session where

import Prelude

import AWS.DynamoDB (DYNAMO, DynamoClient, deleteRecord, loadRecord, saveRecord)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read, write, writeJSON)

