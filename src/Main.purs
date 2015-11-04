module Main where

import Prelude
import Debug.Trace
import Data.Functor (($>), (<$))
import Data.Either (Either())
import Control.Apply ((*>), (<*))
import Data.String (contains, toLower)
import Data.Array (filter, findIndex, index, updateAt, insert)
import Control.Alt ((<|>))
import Control.Plus (Plus)
import Data.Generic (Generic, gEq, gCompare)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson, gEncodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeMaybe, gDecodeJson)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Control.Monad.Aff (Aff(), runAff, forkAff)
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.NaturalTransformation (Natural())
import DOM

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as EH

import qualified LocalStorage as L

main :: forall eff. Eff (HalogenEffects (localStorage :: L.LOCALSTORAGE | eff)) Unit
main = runAff throwException (const (pure unit)) do
  contacts <- liftEff load
  app <- runUI contactsAppComponent (installedState (makeInitialContactsApp contacts))
  appendToBody app.node

becky :: Contact
becky = Contact { name: "Becky Conning"
                , telephone: "+447454809211"
                , email: "becky.conning@icloud.com"
                }

hasName :: String -> Contact -> Boolean
hasName name (Contact contact) = contact.name == name

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

data Route = Present String
           | Edit String
           | List

type ContactsApp = { contacts :: Array Contact, route :: Route }

makeInitialContactsApp :: Array Contact -> ContactsApp
makeInitialContactsApp contacts = { contacts: contacts, route: List }

initialContact :: Contact
initialContact = Contact { name: ""
                         , telephone: ""
                         , email: ""
                         }

type Effects eff = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff)

type ContactsAppDSL eff = Aff (localStorage :: L.LOCALSTORAGE | eff)

type ContactsAppChild = Either ContactsList ContactState
type ContactsAppChildQuery = Coproduct ListQuery EditQuery
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

pathToEdit :: ChildPath ContactState ContactsAppChild EditQuery ContactsAppChildQuery EditSlotAddress ContactsAppChildSlotAddress
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
        { component: editComponent
        , initialState:
            { edit: true
            , contact: findContact name contactsApp.contacts
            }
        }
    renderRoute (Present name) =
      H.slot' pathToEdit EditSlotAddress \_ ->
        { component: editComponent
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

  peekEdit :: forall a. EditQuery a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peekEdit (Done contact _) = saveContact *> pure unit
    where
    saveContact = liftH (liftEff' load) >>= (insertContact contact >>> save >>> liftEff' >>> liftH)
  peekEdit (Back _) = loadAndSetRoute List *> pure unit
  peekEdit _ = pure unit

type ContactsList = { contacts :: Array Contact, search :: String }

data ListQuery next = Search String next
                    | PresentContact String next
                    | CreateContact next

search :: String -> Array Contact -> Array Contact
search "" = id
search s = filter predicate
  where
  predicate :: Contact -> Boolean
  predicate (Contact contact) = contains (toLower s) $ toLower $ contact.name

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

data EditQuery next = Done Contact next
                    | Back next
                    | Cancel next
                    | EditContact next
                    | ChangeName Contact String next
                    | ChangeTelephone Contact String next
                    | ChangeEmail Contact String next

changeName :: String -> Contact -> Contact
changeName value (Contact obj) = Contact $ obj { name = value }

changeTelephone :: String -> Contact -> Contact
changeTelephone value (Contact obj) = Contact $ obj { telephone = value }

changeEmail :: String -> Contact -> Contact
changeEmail value (Contact obj) = Contact $ obj { email = value }

editComponent :: forall eff. Component ContactState EditQuery (ContactsAppDSL eff)
editComponent = component render eval
  where
  render :: ContactState -> ComponentHTML EditQuery
  render contactState | contactState.edit = renderEditContact contactState.contact
  render contactState | otherwise = renderCancel contactState.contact

  renderEditContact :: Contact -> ComponentHTML EditQuery
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

  renderCancel :: Contact -> ComponentHTML EditQuery
  renderCancel (Contact contact) =
    H.div_
      [ H.div_
          [ H.button [ E.onClick (E.input_ Back ) ] [ H.text "Back" ]
          , H.button [ E.onClick (E.input_ EditContact) ] [ H.text "Edit" ]
          ]
      , H.h1_ [ H.text $ contact.name ]
      , renderDetail "Telephone" contact.telephone
      , renderDetail "Email" contact.email
      ]

  renderDetail :: String -> String -> ComponentHTML EditQuery
  renderDetail heading value =
    H.div_ [ H.h3_ [ H.text heading ], H.p_ [ H.text value ] ]

  eval :: Natural EditQuery (ComponentDSL ContactState EditQuery (ContactsAppDSL eff))
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

type ContactState = { contact :: Contact, edit :: Boolean }

newtype Contact = Contact { name :: String
                          , telephone :: String
                          , email :: String
                          }

derive instance genericContact :: Generic Contact

instance eqContact :: Eq Contact where
  eq (Contact obj1) (Contact obj2) = obj1.name == obj2.name
instance ordContact :: Ord Contact where
  compare (Contact obj1) (Contact obj2) = compare obj1.name obj2.name
instance encodeJsonContact :: EncodeJson Contact where encodeJson = gEncodeJson
instance decodeJsonContact :: DecodeJson Contact where decodeJson = gDecodeJson

save :: forall eff. Array Contact -> Eff (localStorage :: L.LOCALSTORAGE | eff) Unit
save = L.set "contacts"

load :: forall eff. Eff (localStorage :: L.LOCALSTORAGE | eff) (Array Contact)
load = L.get "contacts" >>= fromMaybe [] >>> pure

