-- src/Main.purs
-- 2018-01-01 

module Main where

import Prelude
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)

--------

unwrapMaybe :: Maybe String -> String
unwrapMaybe Nothing = ""
unwrapMaybe (Just x) = x

randomElement :: ∀ e. Array String -> Eff (random :: RANDOM | e) String
randomElement arr = unwrapMaybe <$> ranIndex arr
  where
    ranIndex :: forall a e. Array a -> Eff (random :: RANDOM | e) (Maybe a)
    ranIndex xs = map (index xs) (randomInt 0 $ length xs)

f :: ∀ e. Array (Array String) -> Array (Eff (random :: RANDOM | e) String)
f = map randomElement

consonant :: Array String
consonant = ["t", "d", "s", "j", "k", "g", "h", "b", "p", "m", "n", "r"]

vowel :: Array String
vowel = ["a", "i", "e", "u", "o"]

--------

--------
-- main :: Eff _ Unit
main = do
  result <- randomElement ["abc", "def", "ghi"]
  logShow result

-- The End
