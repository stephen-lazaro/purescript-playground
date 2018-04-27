module Four where

import Prelude
import Data.List (List, filter, length)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

isEvenLocal :: Int -> Boolean
isEvenLocal x = isEvenLocalAcc 2
  where
    isEvenLocalAcc :: Int -> Boolean
    isEvenLocalAcc y | y < x = isEvenLocalAcc (2 + y)
                     | y == x = true
                     | otherwise = false

allEvenLocal :: List Int -> Int
allEvenLocal = (filter isEvenLocal) >>> length


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
