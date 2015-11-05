module ContactsList.Component where

import Prelude

import Control.Apply ((*>), (<*))

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import ContactsApp.DSL (ContactsAppDSL())

import ContactsList.Model
import ContactsList.Query

import ContactsApp.Model (search)

import Contact.Model (Contact(..))
import Contact.Component (renderField')

listComponent :: forall eff. Component ContactsList ListQuery (ContactsAppDSL eff)
listComponent = component render eval
  where
  render :: ContactsList -> ComponentHTML ListQuery
  render contactsList =
    H.div_
      [ H.div_ [ H.button [ E.onClick (E.input_ CreateContact) ] [ H.text "Create" ] ]
      , H.h1_ [ H.text "All Contacts" ]
      , renderField' "Search" contactsList.search Search
      , H.ul_ $ renderContact <$> (search contactsList.search contactsList.contacts)
      ]

  renderContact :: Contact -> ComponentHTML ListQuery
  renderContact contact@(Contact obj) =
    H.li_
      [ H.button
          [ E.onClick (E.input_ (PresentContact obj.name)) ]
          [ H.text obj.name ]
      ]

  eval :: Natural ListQuery (ComponentDSL ContactsList ListQuery (ContactsAppDSL eff))
  eval (Search s next) = modify (_ { search = s }) *> pure next
  eval (CreateContact next) = pure next
  eval (PresentContact contact next) = pure next
