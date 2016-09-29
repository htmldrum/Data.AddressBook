module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

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

type AddressBook = List Entry -- linked list of entries

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ". " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.state

emptyBook :: AddressBook
emptyBook = empty
