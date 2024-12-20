module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.ST as ArrayST
import Data.Array.ST as STArray
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Ord.Generic (genericCompare)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Rybl.Data.Variant (Variant)
import Rybl.Halogen.Style (U)

type ViewMode = Variant
  ( unknown :: U
  , mobile :: U
  , wide_desktop :: U
  , narrow_desktop :: U
  )

data Doc
  = String String
  | Error Doc
  | Group GroupStyle (Array Doc)
  | Ref Ref
  | Link Link
  | Expander ExpanderStyle Doc Doc
  | Sidenote Id Doc Doc
  | SidenotesThreshold Doc

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

type Ref = String

type Id = String

type Link = Variant
  ( external :: { label :: String, href :: String, mb_favicon_src :: Maybe String }
  , ref :: { label :: String, ref :: Ref }
  )

type GroupStyle = Variant (row :: U, column :: U, flow :: U)

type ExpanderStyle = Variant (inline :: U, block :: U)

kids_Doc :: Doc -> Array Doc
kids_Doc (String _) = []
kids_Doc (Error d) = [ d ]
kids_Doc (Group _ ds) = ds
kids_Doc (Ref _) = []
kids_Doc (Link _) = []
kids_Doc (Expander _ d1 d2) = [ d1, d2 ]
kids_Doc (Sidenote _ d1 d2) = [ d1, d2 ]
kids_Doc (SidenotesThreshold d) = [ d ]

linearKids_Doc :: Doc -> Array Doc
linearKids_Doc (String _) = []
linearKids_Doc (Error d) = [ d ]
linearKids_Doc (Group _ ds) = ds
linearKids_Doc (Ref _) = []
linearKids_Doc (Link _) = []
linearKids_Doc (Expander _ d _) = [ d ]
linearKids_Doc (Sidenote _ d _) = [ d ]
linearKids_Doc (SidenotesThreshold d) = [ d ]

--------------------------------------------------------------------------------

collectSidenotes :: Doc -> Array { id :: Id, label :: Doc, body :: Doc }
collectSidenotes d0 =
  ArrayST.run do
    sidenotes <- STArray.new
    let
      go d = do
        case d of
          SidenotesThreshold _ -> pure unit -- stop recursing
          Sidenote id label body -> do
            sidenotes # STArray.push { id, label, body } # void
            label # go
          _ -> d # linearKids_Doc # traverse_ go
    go d0
    pure sidenotes

collectRefs :: Doc -> Set Ref
collectRefs d0 =
  ( ArrayST.run do
      refs <- STArray.new
      let
        go d = do
          case d of
            Ref ref -> refs # STArray.push ref # void
            _ -> d # kids_Doc # traverse_ go
      go d0
      pure refs
  ) # Set.fromFoldable
