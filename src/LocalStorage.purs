module LocalStorage where

import Prelude

import Data.Argonaut.Core (Json())
import Control.Monad.Eff (Eff())

foreign import data LOCALSTORAGE :: !

foreign import get :: forall eff. String -> (Eff (localStorage :: LOCALSTORAGE | eff) Json)

foreign import set :: forall eff. String -> Json -> (Eff (localStorage :: LOCALSTORAGE | eff) Unit)

