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

squaresOfStuff :: List Number -> List Number
squaresOfStuff = map \x -> x * x

filterNegatives :: List Number -> List Number
filterNegatives = filter \x -> x >= 0.0

infixl 0 filter as <$?>

filterNegativesOther :: List Number -> List Number
filterNegativesOther lz = (\x -> x >= 0.0) <$?> lz

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
