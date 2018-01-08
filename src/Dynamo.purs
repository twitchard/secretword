module AWS.DynamoDB where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Simple.JSON (write)

type DynamoOptions =
  { region :: String
  , endpoint :: Maybe String
  , apiVersion :: Maybe String
  }

foreign import data DYNAMO :: Effect
foreign import data DynamoClient :: Type
foreign import _getClient :: Foreign → DynamoClient

getClient :: DynamoOptions → DynamoClient
getClient opts = _getClient (write opts)

foreign import _loadRecord :: ∀ e. DynamoClient → String → Foreign → EffFnAff (dynamo :: DYNAMO | e) Foreign

loadRecord :: ∀ e. DynamoClient → String → Foreign → Aff (dynamo :: DYNAMO | e) Foreign
loadRecord client tableName key =
  _loadRecord client tableName key
    # fromEffFnAff

foreign import _saveRecord :: ∀ e. DynamoClient → String → Foreign → EffFnAff (dynamo :: DYNAMO | e) Unit

saveRecord :: ∀ e. DynamoClient → String → Foreign → Aff (dynamo :: DYNAMO | e) Unit
saveRecord client tableName record =
  _saveRecord client tableName record
    # fromEffFnAff

foreign import _deleteRecord :: ∀ e. DynamoClient → String → Foreign → EffFnAff (dynamo :: DYNAMO | e) Unit

deleteRecord :: ∀ e. DynamoClient → String → Foreign → Aff (dynamo :: DYNAMO | e) Unit
deleteRecord client tableName key =
  _deleteRecord client tableName key 
    # fromEffFnAff
