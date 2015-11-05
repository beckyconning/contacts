module Main where

import Prelude
import Debug.Trace
import Data.Functor (($>), (<$))
import Data.Either (Either())
import Control.Apply ((*>), (<*))
import Data.Array (filter, findIndex, index, updateAt, insert)
import Control.Alt ((<|>))
import Control.Plus (Plus)
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
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as EH

import qualified LocalStorage as L

import ContactsApp.Model (load, makeInitialContactsApp)
import ContactsApp.Component (contactsAppComponent)

type Effects eff = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff)

main :: forall eff. Eff (HalogenEffects (localStorage :: L.LOCALSTORAGE | eff)) Unit
main = runAff throwException (const (pure unit)) do
  contacts <- liftEff load
  app <- runUI contactsAppComponent (installedState (makeInitialContactsApp contacts))
  appendToBody app.node


