module ContactsList.Model where

import Contact.Model (Contact())

type ContactsList = { contacts :: Array Contact, search :: String }
