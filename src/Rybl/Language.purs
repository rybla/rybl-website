module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.ST as ArrayST
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)
import Rybl.Data.Fix (Fix)
import Rybl.Data.Fix as Fix
import Rybl.Data.Variant (Variant, inj', inj'U)
import Rybl.Utility (U)

newtype RefId = RefId String

derive instance Newtype RefId _
derive newtype instance Show RefId
derive newtype instance Eq RefId
derive newtype instance Ord RefId
derive newtype instance EncodeJson RefId
derive newtype instance DecodeJson RefId

type Doc = Fix Doc_

data Doc_ self
  = Section { title :: String } (Array self)
  -- 
  | Paragraph {} (Array self)
  -- 
  | Sentence {} (Array self)
  --
  | Link
      { src ::
          Variant
            ( external :: { href :: String, mb_favicon_src :: Maybe String }
            , internal :: { refId :: RefId }
            )
      }
      self
  | Sidenote {} self self
  | Ref { refId :: RefId }
  | String { style :: StringStyle, value :: String }
  | CodeBlock { value :: String }
  | QuoteBlock {} self
  | MathBlock { value :: String }
  -- TODO: media (images/videos/models/etc)
  | Media
      ( Variant
          ( image :: { src :: String }
          )
      )
  | Error { label :: String } self

derive instance Generic (Doc_ self) _

instance Show self => Show (Doc_ self) where
  show x = genericShow x

-- instance Eq self => Eq (Doc_ self) where
--   eq x = genericEq x

-- instance Ord self => Ord (Doc_ self) where
--   compare x = genericCompare x

instance EncodeJson self => EncodeJson (Doc_ self) where
  encodeJson x = genericEncodeJson x

instance DecodeJson self => DecodeJson (Doc_ self) where
  decodeJson x = genericDecodeJson x

derive instance Functor Doc_
derive instance Foldable Doc_
derive instance Traversable Doc_

type StringStyle = Variant
  ( plain :: U
  , emphasis :: U
  , code :: U
  )

--------------------------------------------------------------------------------
-- Doc constructors
--------------------------------------------------------------------------------

section :: String -> Array Doc -> Doc
section title body = Fix.wrap $ Section { title } body

paragraph :: Array Doc -> Doc
paragraph body = Fix.wrap $ Paragraph {} body

sentence :: Array Doc -> Doc
sentence body = Fix.wrap $ Sentence {} body

link_external :: Doc -> { href :: String, mb_favicon_src :: Maybe String } -> Doc
link_external label external = Fix.wrap $ Link { src: inj' @"external" external } label

link_internal :: Doc -> { refId :: RefId } -> Doc
link_internal label internal = Fix.wrap $ Link { src: inj' @"internal" internal } label

sidenote :: Doc -> Doc -> Doc
sidenote label body = Fix.wrap $ Sidenote {} label body

ref :: RefId -> Doc
ref refId = Fix.wrap $ Ref { refId }

string :: String -> Doc
string value = Fix.wrap $ String { style: inj'U @"plain", value }

string_style :: StringStyle -> String -> Doc
string_style style value = Fix.wrap $ String { style, value }

error :: String -> Doc -> Doc
error label body = Fix.wrap $ Error { label } body

--------------------------------------------------------------------------------

-- collectSidenotes :: Doc -> Array { id :: Id, label :: Doc, body :: Doc }
-- collectSidenotes d0 =
--   ArrayST.run do
--     sidenotes <- STArray.new
--     let
--       go d = do
--         case d of
--           SidenotesThreshold _ -> pure unit -- stop recursing
--           Sidenote id label body -> do
--             sidenotes # STArray.push { id, label, body } # void
--             label # go
--           _ -> d # linearKids_Doc # traverse_ go
--     go d0
--     pure sidenotes

collectRefIds :: Doc -> Set RefId
collectRefIds doc0 =
  ArrayST.run
    ( do
        refIds <- ArrayST.new
        doc0 # Fix.traverse_upwards_ case _ of
          Ref doc -> refIds # ArrayST.push doc.refId # void
          _ -> pure unit
        pure refIds
    )
    # Set.fromFoldable

