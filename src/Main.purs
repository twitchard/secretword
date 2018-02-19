module Main where

import Prelude

import AWS.DynamoDB (DYNAMO, getClient)
import Amazon.Alexa.Handler (makeHandler)
import Control.Monad.Aff (Aff, Error, Fiber, launchAff_)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Manifest (manifest)
import Module (model)
import Simple.JSON (write, writeJSON)
import Skill (handle)
import TestDB (emptyDB)

foreign import args ::
  { bin :: String
  , command :: String
  }

foreign import data STDIN :: Effect

foreign import _readJsonFromStdin :: ∀ eff. EffFnAff (stdin :: STDIN | eff) String
readJsonFromStdin :: ∀ e. Aff (stdin :: STDIN | e) String
readJsonFromStdin = fromEffFnAff _readJsonFromStdin


main :: forall t31. Eff ( random :: RANDOM, stdin :: STDIN, exception :: EXCEPTION, console :: CONSOLE | t31) Unit
main = runCommand
  where
    runCommand
      | args.command == "manifest" = generateManifest
      | args.command == "model" = generateModel
      | args.command == "execute" = handleFromStdin
      | otherwise = printUsage

    generateManifest = log $ writeJSON manifest
    generateModel    = log $ writeJSON model
    printUsage       = log $ "TODO"
    handleFromStdin  = launchAff_ do
      event <- readJsonFromStdin <#> write
      let context = write {}
          db = emptyDB
      result <- map write $ handle db event context
      ( liftEff <<< log <<< writeJSON) result


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
