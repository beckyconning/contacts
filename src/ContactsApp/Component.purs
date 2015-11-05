module ContactsApp.Component where

import Prelude

import Control.Apply ((*>), (<*))
import Data.Either (Either())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P

import Data.Functor.Coproduct (Coproduct(), coproduct, left)

import ContactsApp.DSL
import ContactsApp.Model

import Contact.Model
import Contact.Query (ContactQuery(..))
import Contact.Component

import ContactsList.Component (listComponent)
import ContactsList.Model (ContactsList())
import ContactsList.Query (ListQuery(..))

type ContactsAppChild = Either ContactsList ContactState
type ContactsAppChildQuery = Coproduct ListQuery ContactQuery
type ContactsAppChildSlotAddress = Either ListSlotAddress EditSlotAddress

data ContactsAppQuery next = SetRoute Route next

data ListSlotAddress = ListSlotAddress
data EditSlotAddress = EditSlotAddress

derive instance genericListSlotAddress :: Generic ListSlotAddress
instance eqListSlotAddress :: Eq ListSlotAddress where eq = gEq
instance ordListSlotAddress :: Ord ListSlotAddress where compare = gCompare

derive instance genericEditSlotAddress :: Generic EditSlotAddress
instance eqEditSlotAddress :: Eq EditSlotAddress where eq = gEq
instance ordEditSlotAddress :: Ord EditSlotAddress where compare = gCompare

pathToList :: ChildPath ContactsList ContactsAppChild ListQuery ContactsAppChildQuery ListSlotAddress ContactsAppChildSlotAddress
pathToList = cpL

pathToEdit :: ChildPath ContactState ContactsAppChild ContactQuery ContactsAppChildQuery EditSlotAddress ContactsAppChildSlotAddress
pathToEdit = cpR

type ContactsAppP g = InstalledState ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery g ContactsAppChildSlotAddress

type ContactsAppQueryP = Coproduct ContactsAppQuery (ChildF ContactsAppChildSlotAddress ContactsAppChildQuery)

contactsAppComponent :: forall eff. Component (ContactsAppP (ContactsAppDSL eff)) ContactsAppQueryP (ContactsAppDSL eff)
contactsAppComponent = parentComponent' render eval peek
  where
  render :: ContactsApp -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress
  render contactsApp = H.div_ [ renderRoute contactsApp.route ]
    where
    renderRoute :: Route -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress
    renderRoute List =
      H.slot' pathToList ListSlotAddress \_ ->
        { component: listComponent, initialState: { contacts: contactsApp.contacts, search: "" } }
    renderRoute (Edit name) =
      H.slot' pathToEdit EditSlotAddress \_ ->
        { component: contactComponent
        , initialState:
            { edit: true
            , contact: findContact name contactsApp.contacts
            }
        }
    renderRoute (Present name) =
      H.slot' pathToEdit EditSlotAddress \_ ->
        { component: contactComponent
        , initialState:
            { edit: false
            , contact: findContact name contactsApp.contacts
            }
        }

  loadAndSetRoute :: Route -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  loadAndSetRoute route =
    liftH (liftEff' load)
      >>= \contacts -> modify (const { contacts: contacts, route: route })

  eval :: Natural ContactsAppQuery (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress)
  eval (SetRoute route next) = loadAndSetRoute route *> pure next

  peek :: forall a. (ChildF ContactsAppChildSlotAddress ContactsAppChildQuery) a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peek (ChildF _ q) = coproduct peekList peekEdit q

  peekList :: forall a. ListQuery a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peekList (CreateContact _) = modify (_ { route = Edit "" }) *> pure unit
  peekList (PresentContact contact _) = loadAndSetRoute (Present contact) *> pure unit
  peekList _ = pure unit

  peekEdit :: forall a. ContactQuery a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peekEdit (Done contact _) = saveContact *> pure unit
    where
    saveContact = liftH (liftEff' load) >>= (insertContact contact >>> save >>> liftEff' >>> liftH)
  peekEdit (Back _) = loadAndSetRoute List *> pure unit
  peekEdit _ = pure unit
