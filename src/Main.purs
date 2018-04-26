module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.List (List)
import Math (pi)

circleArea :: Number -> Number
circleArea r = pi * r * r

flip :: forall a b c. (a -> b -> c) -> (b -> a -> c)
flip f y x = f x y

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry {firstName, lastName, address} =
  firstName <> " " <> lastName <> ", " <> showAddress address

showAddress :: Address -> String
showAddress {street, city, state} =
  street <> ", " <> city <> " " <> state

showAddressBook :: AddressBook -> String
showAddressBook addressees =
  foldl (\x y -> x <> y) "" (map showEntry addressees)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
