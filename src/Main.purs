module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl)
import Data.List (List(..), head, nubBy, length)
import Data.Maybe (Maybe)
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

--- Were they data... :
--instance eqAddr :: Eq Address where
--  eq addressA addressB =
--    addressA.street == addressB.street &&
--      addressA.city == addressB.city &&
--      addressA.state == addressB.state

--instance eqEntry :: Eq Entry where
--  eq recordA recordB =
--    recordA.firstName == recordB.firstName &&
--      recordA.lastName == recordB.lastName &&
--      recordA.address == recordB.address

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

prependEntry:: Entry -> AddressBook -> AddressBook
prependEntry = Cons

filter :: (Entry -> Boolean) -> AddressBook -> AddressBook
filter f Nil = Nil
filter f (Cons hd rst) = case (f hd) of
  true -> Cons hd (filter f rst)
  false -> filter f rst

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry =
      entry.firstName == firstName &&
        entry.lastName == lastName

findByAddress :: Address -> AddressBook -> Maybe Entry
findByAddress address = head <<< filter findAddress
  where
    findAddress :: Entry -> Boolean
    findAddress entry =
      entry.address.street == address.street &&
        entry.address.city == address.city &&
        entry.address.state == address.state

-- Really want cartesian product
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy eqEntry
  where
    eqEntry :: Entry -> Entry -> Boolean
    eqEntry entryA entryB = 
      entryA.firstName == entryB.firstName &&
        entryA.lastName == entryB.lastName &&
        entryA.address.street == entryB.address.street &&
        entryA.address.city == entryB.address.city &&
        entryA.address.state == entryB.address.state

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
