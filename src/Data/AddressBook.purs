module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null)
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

insertEntry :: Entry -> AddressBook -> AddressBook
-- insertEntry entry ab = Cons entry ab
-- Eta convert out the collection
-- insertEntry entry = Cons entry
-- Eta convert out the collection
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
-- :> :type filter
-- forall a. (a -> Boolean) -> List a -> List a
-- findEntry firstName lastName ab = head $ filter filterEntry ab -- using infix operator
-- findEntry firstName lastName ab = (head <<< filter filterEntry) ab -- using backwards composition - head takes the composition of filter and filterEntry
-- head :: AddressBook -> Maybe Entry
-- filter :: (Entry -> Boolean) -> List Entry -> List Entry
findEntry firstName lastName = head <<< filter filterEntry -- eta conversion on ab
                                                           -- findEntry is the composition of a filtering function and the head function
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.lastName == lastName && entry.firstName == firstName -- closed-over variables

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName ab = head $ filter filterEntry ab
  where
    filterEntry :: Entry -> Boolean
    filterEntry e = e.address.street == streetName

-- TODO: Learn how to write multiple function definitions for different cases
-- nameInAddressBook :: String -> String -> AddressBook -> Boolean
-- nameInAddressBook firstName lastName ab = null ab then false

-- if ab is empty, return false
-- if ab is not empty, take head, see if it passes predicate, if it doesnt call self with rest

-- TODO: removeDuplies, remove entries with same first and Last Name
-- removeDuplicates :: AddressBook -> AddressBook
