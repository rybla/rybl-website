module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)

data Doc
  = String String
  | Group GroupStyle (Array Doc)
  | Ref String

derive instance Generic Doc _

instance Show Doc where
  show x = genericShow x

instance EncodeJson Doc where
  encodeJson x = genericEncodeJson x

instance DecodeJson Doc where
  decodeJson x = genericDecodeJson x

type GroupStyle = Variant (row :: Unit, column :: Unit)

