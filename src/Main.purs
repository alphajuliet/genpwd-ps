-- src/Main.purs
-- 2018-01-01 

module Main where

import Prelude
import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Foldable (foldr)
import Data.Unfoldable (replicateA)

--------------------------------

type RandomString = ∀ e. Eff (random :: RANDOM | e) String


-- Unwrap the Maybe from a string

unwrapMaybe :: Maybe String -> String
unwrapMaybe Nothing = ""
unwrapMaybe (Just x) = x


-- Return a random string from an array of strings

randomElement :: Array String -> RandomString
randomElement arr = unwrapMaybe <$> ranIndex arr
  where
    ranIndex :: ∀ a e. Array a -> Eff (random :: RANDOM | e) (Maybe a)
    ranIndex xs = map (\n -> index xs (n-1)) (randomInt 0 $ length xs)

-- Concatenate an array of strings
-- concat :: ∀ f. f String -> String
concat = foldr append ""

--------------------------------
-- Define source lists

consonant :: Array String
consonant = ["t", "d", "s", "j", "k", "g", "h", "b", "p", "m", "n", "r", "z",
  "v", "br", "kr", "tr", "sm", "st"]

vowel :: Array String
vowel = ["a", "i", "e", "u", "o"]

pairs :: Array String
pairs = append <$> consonant <*> vowel

genWord :: ∀ e. Int -> Eff (random :: RANDOM | e) (Array String)
genWord len = replicateA len (randomElement pairs)

{-- generateWord :: RandomString --}
{-- generateWord =  [consonant, vowel] --}


--------------------------------
-- main :: Eff _ Unit
main = do
  result <- concat <$> genWord 4
  logShow result

-- The End
