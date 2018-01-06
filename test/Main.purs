module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Foreign (Foreign)
import Main (myHandler)

foreign import test1 :: Foreign
foreign import log :: ∀ e. Foreign → Eff (console :: CONSOLE | e) Unit

main = launchAff $ do
  result <- myHandler test1 test1
  liftEff $ log result
