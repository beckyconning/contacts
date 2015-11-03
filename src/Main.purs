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
import Control.Monad.Eff.Class (MonadEff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.NaturalTransformation (Natural())
import DOM
import Routing.Match
import Routing.Match.Class
import Routing

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as EH

import qualified LocalStorage as L

becky :: Contact
becky = Contact { name: "Becky Conning"
                , pronouns: "She/her/hers"
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

initialContactsApp :: ContactsApp
initialContactsApp = { contacts: [], route: List }

initialContact :: Contact
initialContact = Contact { name: ""
                         , pronouns: ""
                         , telephone: ""
                         , email: ""
                         }

matchRoute :: Match Route
matchRoute = edit <|> present <|> list
  where
  edit = (\s _ -> Edit s) <$> (lit "" *> str) <*> lit "edit"
  present = Present <$> (lit "" *> str)
  list = List <$ lit ""

type Effects eff = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff)

type ContactsAppDSL eff = Aff (localStorage :: L.LOCALSTORAGE | eff)

redirects :: forall eff. Driver ContactsAppQueryP eff -> Maybe Route -> Route -> Aff (Effects eff) Unit
redirects driver _ = driver <<< left <<< action <<< SetRoute

routeSignal :: forall eff. Driver ContactsAppQueryP eff -> Aff (Effects eff) Unit
routeSignal driver = matchesAff matchRoute >>= route
  where
  route (Tuple old new) = redirects driver old new

type ContactsAppChild = Either ContactsList (Either Contact Contact)
type ContactsAppChildQuery = Coproduct ListQuery (Coproduct EditQuery PresentQuery)
type ContactsAppChildSlotAddress = Either ListSlotAddress (Either EditSlotAddress PresentSlotAddress)

data ContactsAppQuery next = SetRoute Route next

data ListSlotAddress = ListSlotAddress
data EditSlotAddress = EditSlotAddress
data PresentSlotAddress = PresentSlotAddress

derive instance genericListSlotAddress :: Generic ListSlotAddress
instance eqListSlotAddress :: Eq ListSlotAddress where eq = gEq
instance ordListSlotAddress :: Ord ListSlotAddress where compare = gCompare

derive instance genericEditSlotAddress :: Generic EditSlotAddress
instance eqEditSlotAddress :: Eq EditSlotAddress where eq = gEq
instance ordEditSlotAddress :: Ord EditSlotAddress where compare = gCompare

derive instance genericPresentSlotAddress :: Generic PresentSlotAddress
instance eqPresentSlotAddress :: Eq PresentSlotAddress where eq = gEq
instance ordPresentSlotAddress :: Ord PresentSlotAddress where compare = gCompare

pathToList :: ChildPath ContactsList ContactsAppChild ListQuery ContactsAppChildQuery ListSlotAddress ContactsAppChildSlotAddress
pathToList = cpL

pathToEdit :: ChildPath Contact ContactsAppChild EditQuery ContactsAppChildQuery EditSlotAddress ContactsAppChildSlotAddress
pathToEdit = cpR :> cpL

pathToPresent :: ChildPath Contact ContactsAppChild PresentQuery ContactsAppChildQuery PresentSlotAddress ContactsAppChildSlotAddress
pathToPresent = cpR :> cpR

type ContactsAppP g = InstalledState ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery g ContactsAppChildSlotAddress

type ContactsAppQueryP = Coproduct ContactsAppQuery (ChildF ContactsAppChildSlotAddress ContactsAppChildQuery)

contactsAppComponent :: forall eff. Component (ContactsAppP (ContactsAppDSL eff)) ContactsAppQueryP (ContactsAppDSL eff)
contactsAppComponent = parentComponent' render eval peek
  where
  render :: ContactsApp -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress
  render contactsApp =
    H.div [ ] [ renderRoute contactsApp.route ]
      where
      renderRoute :: Route -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress
      renderRoute List =
        H.slot' pathToList ListSlotAddress \_ ->
          { component: listComponent, initialState: { contacts: contactsApp.contacts, search: "" } }
      renderRoute (Edit name) =
        H.slot' pathToEdit EditSlotAddress \_ ->
          { component: editComponent, initialState: findContact name contactsApp.contacts }
      renderRoute (Present name) =
        H.slot' pathToPresent PresentSlotAddress \_ ->
          { component: presentComponent, initialState: findContact name contactsApp.contacts }

  eval :: Natural ContactsAppQuery (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress)
  eval (SetRoute (Present name) next) = modify (_ { route = Present name }) $> next
  eval (SetRoute (Edit name) next) = modify (_ { route = Edit name }) $> next
  eval (SetRoute List next) = modify (_ { route = List }) $> next

  peek :: forall a. (ChildF ContactsAppChildSlotAddress ContactsAppChildQuery) a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peek (ChildF _ q) = coproduct (const (pure unit)) (coproduct peekEdit (const (pure unit))) q

  peekEdit :: forall a. EditQuery a -> (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery (ContactsAppDSL eff) ContactsAppChildSlotAddress) Unit
  peekEdit (SaveContact contact _) = saveContact *> pure unit
    where
    saveContact = liftH (liftEff' load) >>= (insertContact contact >>> save >>> liftEff' >>> liftH)
  peekEdit _ = pure unit

main :: forall eff. Eff (HalogenEffects (localStorage :: L.LOCALSTORAGE | eff)) Unit
main = runAff throwException (const (pure unit))
  $ runUI contactsAppComponent (installedState initialContactsApp)
  >>= \app -> appendToBody app.node *> forkAff (routeSignal app.driver)

type ContactsList = { contacts :: Array Contact, search :: String }

data ListQuery next = Search String next
                    | LoadContacts next

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
    H.div
      [ P.initializer \_ -> action LoadContacts ]
      [ H.div_ [ H.a [ P.href "#//edit" ] [ H.text "Add" ] ]
      , H.h1_ [ H.text "All Contacts" ]
      , renderField' "Search" contactsList.search Search
      , H.ul_ $ renderContact <$> (search contactsList.search contactsList.contacts)
      ]

  renderContact :: Contact -> ComponentHTML ListQuery
  renderContact (Contact contact) =
    H.li_ [ H.a [ P.href ("#/" ++ contact.name) ] [ H.text contact.name ] ]

  eval :: Natural ListQuery (ComponentDSL ContactsList ListQuery (ContactsAppDSL eff))
  eval (Search s next) = modify (_ { search = s }) $> next
  eval (LoadContacts next) =
    liftEff' load >>= \contacts -> modify (const { contacts: contacts, search: "" }) $> next

data EditQuery next = SaveContact Contact next
                    | ChangeName String next
                    | ChangePronouns String next
                    | ChangeTelephone String next
                    | ChangeEmail String next

changeName :: String -> Contact -> Contact
changeName value (Contact obj) = Contact $ obj { name = value }

changePronouns :: String -> Contact -> Contact
changePronouns value (Contact obj) = Contact $ obj { pronouns = value }

changeTelephone :: String -> Contact -> Contact
changeTelephone value (Contact obj) = Contact $ obj { telephone = value }

changeEmail :: String -> Contact -> Contact
changeEmail value (Contact obj) = Contact $ obj { email = value }

editComponent :: forall g. (Functor g) => Component Contact EditQuery g
editComponent = component render eval
  where
  render :: Contact -> ComponentHTML EditQuery
  render contact@(Contact obj) =
    H.div_
      [ H.div_
          [ H.a [ P.href ("#/" ++ obj.name) ] [ H.text "Cancel" ]
          , H.button [ E.onClick (E.input_ (SaveContact contact)) ] [ H.text "Done" ]
          ]
      , renderField' "Name" obj.name ChangeName
      , renderField' "Pronouns" obj.pronouns ChangePronouns
      , renderField' "Telephone" obj.telephone ChangeTelephone
      , renderField' "Email" obj.email ChangeEmail
      ]

  eval :: Natural EditQuery (ComponentDSL Contact EditQuery g)
  eval (SaveContact contact next) = pure next
  eval (ChangeName value next) = modify (changeName value) $> next
  eval (ChangePronouns value next) = modify (changePronouns value) $> next
  eval (ChangeTelephone value next) = modify (changeTelephone value) $> next
  eval (ChangeEmail value next) = modify (changeEmail value) $> next

data PresentQuery next = PresentNoop next

presentComponent :: forall g. (Functor g) => Component Contact PresentQuery g
presentComponent = component render eval
  where
  render :: Contact -> ComponentHTML PresentQuery
  render (Contact contact) =
    H.div_
      [ H.div_
          [ H.a [ P.href ("#") ] [ H.text "Back" ]
          , H.a [ P.href ("#/" ++ contact.name ++ "/edit") ] [ H.text "Edit" ]
          ]
      , H.h1_ [ H.text $ contact.name ++ " (" ++ contact.pronouns ++ ")" ]
      , renderDetail "Telephone" contact.telephone
      , renderDetail "Email" contact.email
      ]

  renderDetail :: String -> String -> ComponentHTML PresentQuery
  renderDetail heading value =
    H.div_ [ H.h3_ [ H.text heading ], H.p_ [ H.text value ] ]

  eval :: Natural PresentQuery (ComponentDSL Contact PresentQuery g)
  eval (PresentNoop next) = pure next

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

newtype Contact = Contact { name :: String
                          , pronouns :: String
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

