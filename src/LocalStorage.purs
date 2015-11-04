module LocalStorage where

import Prelude

import Data.Argonaut.Core (Json())
import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Decode (DecodeJson, decodeMaybe)
import Data.Maybe (Maybe(..))
import Data.Either (Either(), either)

foreign import data LOCALSTORAGE :: !

rightToMaybe :: forall a b. Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

get :: forall eff a. (DecodeJson a) => String -> Eff (localStorage :: LOCALSTORAGE | eff) (Maybe a)
get key = getImpl key >>= jsonParser >>> (rightToMaybe >=> decodeMaybe) >>> pure

set :: forall eff a. (EncodeJson a) => String -> a -> Eff (localStorage :: LOCALSTORAGE | eff) Unit
set key = encodeJson >>> printJson >>> setImpl key

foreign import getImpl :: forall eff. String -> (Eff (localStorage :: LOCALSTORAGE | eff) String)

foreign import setImpl :: forall eff. String -> String -> (Eff (localStorage :: LOCALSTORAGE | eff) Unit)
