module Main where

import Prelude
import Data.Functor (($>), (<$))
import Data.Either (Either())
import Control.Apply ((*>), (<*))
import Data.String (contains, toLower)
import Data.Array (filter)
import Control.Alt ((<|>))
import Control.Plus (Plus)
import Data.Generic (Generic, gEq, gCompare)
import Data.Argonaut.Encode (EncodeJson, encodeJson, gEncodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeMaybe, gDecodeJson)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import Control.Monad.Aff (Aff(), runAff, forkAff)
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff)
import Data.Maybe (Maybe(..), maybe)
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

import qualified LocalStorage as L

becky :: Contact
becky = Contact { name: "Becky Conning"
                , pronouns: "She/her/hers"
                , telephone: "+447454809211"
                , email: "becky.conning@icloud.com"
                }

data Route = Present String
           | Edit String
           | List

type ContactsApp = { contacts :: Array Contact, route :: Route }

initialContactsApp :: ContactsApp
initialContactsApp = { contacts: [], route: List }

matchRoute :: Match Route
matchRoute = edit <|> present <|> list
  where
  edit = (\s _ -> Edit s) <$> (lit "" *> str) <*> lit "edit"
  present = Present <$> (lit "" *> str)
  list = List <$ lit ""

type Effects eff = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff)

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

contactsAppComponent :: forall g. (Plus g) => Component (ContactsAppP g) ContactsAppQueryP g
contactsAppComponent = parentComponent render eval
  where
  render :: ContactsApp -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery g ContactsAppChildSlotAddress
  render contactsApp =
    H.div_ [ renderRoute contactsApp.route ]

  renderRoute :: Route -> ParentHTML ContactsAppChild ContactsAppQuery ContactsAppChildQuery g ContactsAppChildSlotAddress
  renderRoute List =
    H.slot' pathToList ListSlotAddress \_ ->
      { component: listComponent, initialState: { contacts: [ becky ], search: "" } }
  renderRoute (Edit name) =
    H.slot' pathToEdit EditSlotAddress \_ ->
      { component: editComponent, initialState: becky }
  renderRoute (Present name) =
    H.slot' pathToPresent PresentSlotAddress \_ ->
      { component: presentComponent, initialState: becky }

  eval :: Natural ContactsAppQuery (ParentDSL ContactsApp ContactsAppChild ContactsAppQuery ContactsAppChildQuery g ContactsAppChildSlotAddress)
  eval (SetRoute (Present name) next) = modify (_ { route = Present name }) $> next
  eval (SetRoute (Edit name) next) = modify (_ { route = Edit name }) $> next
  eval (SetRoute List next) = modify (_ { route = List }) $> next

main :: forall eff. Eff (Effects eff) Unit
main = runAff throwException (const (pure unit))
  $ runUI contactsAppComponent (installedState initialContactsApp)
  >>= \app -> appendToBody app.node *> forkAff (routeSignal app.driver)

type ContactsList = { contacts :: Array Contact, search :: String }

data ListQuery next = Search String next

search :: String -> Array Contact -> Array Contact
search "" = id
search s = filter predicate
  where
  predicate :: Contact -> Boolean
  predicate (Contact contact) = contains (toLower s) $ toLower $ contact.name

listComponent :: forall g. (Functor g) => Component ContactsList ListQuery g
listComponent = component render eval
  where
  render :: ContactsList -> ComponentHTML ListQuery
  render contactsList =
    H.div_
      [ H.h1_ [ H.text "All Contacts" ]
      , renderField' "Search" contactsList.search Search
      , H.ul_ $ renderContact <$> (search contactsList.search contactsList.contacts)
      ]

  renderContact :: Contact -> ComponentHTML ListQuery
  renderContact (Contact contact) =
    H.li_ [ H.a [ P.href ("#/" ++ contact.name) ] [ H.text contact.name ] ]

  eval :: Natural ListQuery (ComponentDSL ContactsList ListQuery g)
  eval (Search s next) = modify (_ { search = s }) $> next

data EditQuery next = EditNoop next

editComponent :: forall g. (Functor g) => Component Contact EditQuery g
editComponent = component render eval
  where
  render :: Contact -> ComponentHTML EditQuery
  render (Contact contact) =
    H.div_
      [ H.div_ [ H.a [ P.href ("#/" ++ contact.name) ] [ H.text "Cancel" ] ]
      , renderField "Name" contact.name
      , renderField "Pronouns" contact.pronouns
      , renderField "Telephone" contact.telephone
      , renderField "Email" contact.email
      ]

  eval :: Natural EditQuery (ComponentDSL Contact EditQuery g)
  eval (EditNoop next) = pure next

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

instance encodeJsonContact :: EncodeJson Contact where
  encodeJson = gEncodeJson

instance decodeJsonContact :: DecodeJson Contact where
  decodeJson = gDecodeJson

save :: forall eff. Array Contact -> Eff (localStorage :: L.LOCALSTORAGE | eff) Unit
save = L.set "contacts" <<< encodeJson

load :: forall eff. Eff (localStorage :: L.LOCALSTORAGE | eff) (Maybe (Array Contact))
load = L.get "contacts" >>= pure <<< decodeMaybe

