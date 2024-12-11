module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.ST (STArray)
import Data.Array.ST as ArrayST
import Data.Array.ST as STArray
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Variant (Variant)

data Doc
  = String String
  | Group GroupStyle (Array Doc)
  | Ref Ref

derive instance Generic Doc _

instance Show Doc where
  show x = genericShow x

instance EncodeJson Doc where
  encodeJson x = genericEncodeJson x

instance DecodeJson Doc where
  decodeJson x = genericDecodeJson x

type Ref = String

type GroupStyle = Variant (row :: Unit, column :: Unit)

--------------------------------------------------------------------------------

collectRefs :: Doc -> Set Ref
collectRefs d0 =
  ( ArrayST.run do
      refs :: STArray _ String <- STArray.new
      let
        go (String _) = pure unit
        go (Group _ ds) = ds # traverse_ go
        go (Ref ref) = refs # STArray.push ref # void
      go d0
      pure refs
  ) # Set.fromFoldable
