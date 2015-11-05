module Contact.Component where

import Prelude

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Control.Apply ((*>), (<*))

import Contact.Model
import Contact.Query

import ContactsApp.DSL

contactComponent :: forall eff. Component ContactState ContactQuery (ContactsAppDSL eff)
contactComponent = component render eval
  where
  render :: ContactState -> ComponentHTML ContactQuery
  render contactState | contactState.edit = renderEditContact contactState.contact
  render contactState | otherwise = renderPresent contactState.contact

  renderEditContact :: Contact -> ComponentHTML ContactQuery
  renderEditContact contact@(Contact obj) =
    H.div_
      [ H.div_
          [ H.button [ E.onClick (E.input_ Cancel ) ] [ H.text "Cancel" ]
          , H.button [ E.onClick (E.input_ (Done contact)) ] [ H.text "Done" ]
          ]
      , renderField' "Name" obj.name (ChangeName contact)
      , renderField' "Telephone" obj.telephone (ChangeTelephone contact)
      , renderField' "Email" obj.email (ChangeEmail contact)
      ]

  renderPresent :: Contact -> ComponentHTML ContactQuery
  renderPresent (Contact contact) =
    H.div_
      [ H.div_
          [ H.button [ E.onClick (E.input_ Back ) ] [ H.text "Back" ]
          , H.button [ E.onClick (E.input_ EditContact) ] [ H.text "Edit" ]
          ]
      , H.h1_ [ H.text $ contact.name ]
      , renderDetail "Telephone" contact.telephone
      , renderDetail "Email" contact.email
      ]

  renderDetail :: String -> String -> ComponentHTML ContactQuery
  renderDetail heading value =
    H.div_ [ H.h3_ [ H.text heading ], H.p_ [ H.text value ] ]

  eval :: Natural ContactQuery (ComponentDSL ContactState ContactQuery (ContactsAppDSL eff))
  eval (Done contact next) = modify (_ { edit = false }) *> pure next
  eval (Cancel next) = modify (_ { edit = false }) *> pure next
  eval (Back next) = pure next
  eval (EditContact next) = modify (_ { edit = true }) *> pure next
  eval (ChangeName contact value next) =
    modify (_ { contact = changeName value contact }) *> pure next
  eval (ChangeTelephone contact value next) =
    modify (_ { contact = changeTelephone value contact }) *> pure next
  eval (ChangeEmail contact value next) =
    modify (_ { contact = changeEmail value contact }) *> pure next

renderField :: forall query. String -> String -> ComponentHTML query
renderField label value =
  H.div_
    [ H.label [ P.for label ] [ H.text label ]
    , H.input [ P.id_ label, P.value value ]
    ]

renderField' :: forall query. String -> String -> (String -> Action query) -> ComponentHTML query
renderField' label value query =
  H.div_
    [ H.label [ P.for label ] [ H.text label ]
    , H.input [ P.id_ label, P.value value, E.onValueInput (E.input query) ]
    ]
