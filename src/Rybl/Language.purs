module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.ST as ArrayST
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Rybl.Data.Variant (Variant, inj', inj'U)
import Rybl.Utility (U)

newtype RefId = RefId String

derive instance Newtype RefId _
derive newtype instance Show RefId
derive newtype instance Eq RefId
derive newtype instance Ord RefId
derive newtype instance EncodeJson RefId
derive newtype instance DecodeJson RefId

data Doc
  = Section { title :: Doc, body :: Array Doc }
  -- 
  | Paragraph { body :: Array Doc }
  -- 
  | Sentence { body :: Array Doc }
  -- 
  | Link
      { label :: Doc
      , src ::
          Variant
            ( external :: { href :: String, mb_favicon_src :: Maybe String }
            , internal :: { refId :: RefId }
            )
      }
  | Sidenote { label :: Doc, body :: Doc }
  | Ref { refId :: RefId }
  | String { style :: StringStyle, value :: String }
  | Error { label :: String, body :: Doc }

derive instance Generic Doc _

instance Show Doc where
  show x = genericShow x

instance Eq Doc where
  eq x = genericEq x

instance Ord Doc where
  compare x = genericCompare x

instance EncodeJson Doc where
  encodeJson x = genericEncodeJson x

instance DecodeJson Doc where
  decodeJson x = genericDecodeJson x

type StringStyle = Variant
  ( plain :: U
  , emphasis :: U
  , code :: U
  )

kids_Doc :: Doc -> Array Doc
kids_Doc (Section doc) = [ doc.title ] <> doc.body
kids_Doc (Paragraph doc) = doc.body
kids_Doc (Sentence doc) = doc.body
kids_Doc (Link doc) = [ doc.label ]
kids_Doc (Sidenote doc) = [ doc.label, doc.body ]
kids_Doc (Ref _) = []
kids_Doc (String _) = []
kids_Doc (Error doc) = [ doc.body ]

--------------------------------------------------------------------------------
-- Doc constructors
--------------------------------------------------------------------------------

section :: Doc -> Array Doc -> Doc
section title body = Section { title, body }

paragraph :: Array Doc -> Doc
paragraph body = Paragraph { body }

sentence :: Array Doc -> Doc
sentence body = Sentence { body }

link_external :: Doc -> { href :: String, mb_favicon_src :: Maybe String } -> Doc
link_external label external = Link { label, src: inj' @"external" external }

link_internal :: Doc -> { refId :: RefId } -> Doc
link_internal label internal = Link { label, src: inj' @"internal" internal }

sidenote :: Doc -> Doc -> Doc
sidenote label body = Sidenote { label, body }

ref :: RefId -> Doc
ref refId = Ref { refId }

string :: String -> Doc
string value = String { style: inj'U @"plain", value }

string_style :: StringStyle -> String -> Doc
string_style style value = String { style, value }

error :: String -> Doc -> Doc
error label body = Error { label, body }

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
collectRefIds d0 =
  ( ArrayST.run do
      refIds <- ArrayST.new
      let
        go doc = do
          case doc of
            Ref doc' -> refIds # ArrayST.push doc'.refId # void
            _ -> doc # kids_Doc # traverse_ go
      go d0
      pure refIds
  ) # Set.fromFoldable
