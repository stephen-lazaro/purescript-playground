module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List (List(..), filter, head)
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



main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
