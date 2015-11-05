module ContactsApp.Model where

import Prelude

import Data.String (contains, toLower)
import Control.Monad.Eff (Eff())
import Data.Array (filter, findIndex, index, updateAt, insert)
import Data.Maybe (Maybe(..), maybe, fromMaybe)

import qualified LocalStorage as L

import Contact.Model

data Route = Present String | Edit String | List

type ContactsApp = { contacts :: Array Contact, route :: Route }

makeInitialContactsApp :: Array Contact -> ContactsApp
makeInitialContactsApp contacts = { contacts: contacts, route: List }

findContact :: String -> Array Contact -> Contact
findContact name contacts = fromMaybe initialContact maybeContact
  where
  maybeContact :: Maybe Contact
  maybeContact = findIndex (hasName name) contacts >>= index contacts

insertContact :: Contact -> Array Contact -> Array Contact
insertContact contact@(Contact obj) contacts = fromMaybe inserted $ maybeIndex >>= replace
  where
  maybeIndex :: Maybe Int
  maybeIndex = findIndex (hasName obj.name) contacts

  replace :: Int -> Maybe (Array Contact)
  replace i = updateAt i contact contacts

  inserted :: Array Contact
  inserted = insert contact contacts

save :: forall eff. Array Contact -> Eff (localStorage :: L.LOCALSTORAGE | eff) Unit
save = L.set "contacts"

load :: forall eff. Eff (localStorage :: L.LOCALSTORAGE | eff) (Array Contact)
load = L.get "contacts" >>= fromMaybe [] >>> pure

search :: String -> Array Contact -> Array Contact
search "" = id
search s = filter predicate
  where
  predicate :: Contact -> Boolean
  predicate (Contact contact) = contains (toLower s) $ toLower $ contact.name

