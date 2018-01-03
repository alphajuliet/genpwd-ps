-- src/Main.purs
-- 2018-01-01 

module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Random (RANDOM, random, randomInt)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Hello sailor!"


-- The End
