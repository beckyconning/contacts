module ContactsApp.DSL where

import Control.Monad.Aff (Aff())
import qualified LocalStorage as L

type ContactsAppDSL eff = Aff (localStorage :: L.LOCALSTORAGE | eff)
