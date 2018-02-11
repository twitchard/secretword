module Main where

import Prelude

import AWS.DynamoDB (DYNAMO, getClient)
import Control.Monad.Aff (Error, Fiber)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Manifest (manifest)
import Module (model)
import Simple.JSON (write, writeJSON)
import Skill (handle)
import Web.AWS.Lambda (makeHandler)

foreign import args ::
  { bin :: String
  , command :: String
  }

main :: forall t31. Eff ( console :: CONSOLE | t31) Unit
main = runCommand
  where
    runCommand
      | args.command == "manifest" = generateManifest
      | args.command == "model" = generateModel
      | otherwise = printUsage

    generateManifest = log $ writeJSON manifest
    generateModel    = log $ writeJSON model
    printUsage       = log $ "TODO"

handler :: forall e.
   EffFn3
     ( console :: CONSOLE , dynamo :: DYNAMO , random :: RANDOM | e)
     Foreign
     Foreign
     (EffFn2
        ( console :: CONSOLE , dynamo :: DYNAMO , random :: RANDOM | e)
        (Nullable Error)
        Foreign
        Unit
     )
     (Fiber
        ( console :: CONSOLE , dynamo :: DYNAMO , random :: RANDOM | e)
        Unit
     )
handler =
  let db = getClient
             { region : "us-east-1"
             , endpoint : Nothing
             , apiVersion : Nothing
             }
  in
  makeHandler $ \event context →
    map write $ handle db event context
