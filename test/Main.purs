module Test.Main where

import Prelude

import AWS.DynamoDB (DYNAMO)
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Data.Foreign (Foreign)
import Skill (handle)

foreign import test1 :: Foreign
foreign import test2 :: Foreign
foreign import log :: ∀ e. Foreign → Eff (console :: CONSOLE | e) Unit

foreign import lambdaTest :: ∀ e. Eff (console :: CONSOLE | e) Unit

main :: forall t8.
   Eff
     ( dynamo :: DYNAMO
     , console :: CONSOLE
     , random :: RANDOM
     | t8
     )
     (Fiber
        ( dynamo :: DYNAMO
        , console :: CONSOLE
        , random :: RANDOM
        | t8
        )
        Unit
     )
main = launchAff $ do
  handle test1 test1 >>= (liftEff <<< log)
  handle test2 test2 >>= (liftEff <<< log)
  --liftEff lambdaTest
