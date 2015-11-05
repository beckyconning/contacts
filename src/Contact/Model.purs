module Contact.Model where

import Prelude

import Data.Generic (Generic, gEq, gCompare)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson, gEncodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeMaybe, gDecodeJson)

type ContactState = { contact :: Contact, edit :: Boolean }

newtype Contact = Contact { name :: String
                          , telephone :: String
                          , email :: String
                          }

initialContact :: Contact
initialContact = Contact { name: ""
                         , telephone: ""
                         , email: ""
                         }

hasName :: String -> Contact -> Boolean
hasName name (Contact contact) = contact.name == name

changeName :: String -> Contact -> Contact
changeName value (Contact obj) = Contact $ obj { name = value }

changeTelephone :: String -> Contact -> Contact
changeTelephone value (Contact obj) = Contact $ obj { telephone = value }

changeEmail :: String -> Contact -> Contact
changeEmail value (Contact obj) = Contact $ obj { email = value }

derive instance genericContact :: Generic Contact

instance eqContact :: Eq Contact where
  eq (Contact obj1) (Contact obj2) = obj1.name == obj2.name

instance ordContact :: Ord Contact where
  compare (Contact obj1) (Contact obj2) = compare obj1.name obj2.name

instance encodeJsonContact :: EncodeJson Contact where
  encodeJson = gEncodeJson

instance decodeJsonContact :: DecodeJson Contact where
  decodeJson = gDecodeJson

