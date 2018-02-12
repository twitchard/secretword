module TestDB where

import Prelude

import DB (class DB)
import Data.Maybe (Maybe(..))
import Types (Session)

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
