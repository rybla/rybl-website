module Rybl.Language where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array.ST as ArrayST
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)
import Prim.Row (class Nub, class Union)
import Record as R
import Rybl.Data.Fix (Fix)
import Rybl.Data.Fix as Fix
import Rybl.Data.Variant (Variant)
import Rybl.Utility (U)

--------------------------------------------------------------------------------
-- Doc
--------------------------------------------------------------------------------

-- | `Doc` is structured as a `Fix` so that map/fold/traverse/etc behavior can be derived automatically.
type Doc = Fix Doc_

data Doc_ self
  = Page (Record PageOpts) (Record PagePrms) (Array self)
  | Section (Record SectionOpts) (Record SectionPrms) (Array self)
  | Paragraph (Record ParagraphOpts) (Record ParagraphPrms) (Array self)
  | Sentence (Record SentenceOpts) (Record SentencePrms) (Array self)
  | LinkExternal (Record LinkExternalOpts) (Record LinkExternalPrms) self
  | LinkInternal (Record LinkInternalOpts) (Record LinkInternalPrms) self
  | Sidenote (Record SidenoteOpts) (Record SidenotePrms) self self
  | Ref (Record RefOpts) (Record RefPrms)
  | String (Record StringOpts) (Record StringPrms)
  | CodeBlock (Record CodeBlockOpts) (Record CodeBlockPrms)
  | QuoteBlock (Record QuoteBlockOpts) (Record QuoteBlockPrms) self
  | MathBlock (Record MathBlockOpts) (Record MathBlockPrms)
  -- TODO: other media (videos/models/etc)
  | Image (Record ImageOpts) (Record ImagePrms) (Maybe self)
  | Error (Record ErrorOpts) (Record ErrorPrms) self

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

type PageOpts = () :: Row Type
type PagePrms = (id :: String, title :: String)

type SectionOpts = () :: Row Type
type SectionPrms = (id :: String, title :: String)

type ParagraphOpts = () :: Row Type
type ParagraphPrms = () :: Row Type

type SentenceOpts = () :: Row Type
type SentencePrms = () :: Row Type

type LinkExternalOpts = (favicon_url :: Maybe String, citation :: Maybe Citation, url :: Maybe String)
type LinkExternalPrms = () :: Row Type

type LinkInternalOpts = (citation :: Maybe Citation, refId :: Maybe RefId)
type LinkInternalPrms = () :: Row Type

type SidenoteOpts = () :: Row Type
type SidenotePrms = () :: Row Type

type RefOpts = () :: Row Type
type RefPrms = (refId :: RefId)

type StringOpts = (style :: Maybe StringStyle)
type StringPrms = (value :: String)

type CodeBlockOpts = (citation :: Maybe Citation)
type CodeBlockPrms = (value :: String)

type QuoteBlockOpts = (citation :: Maybe Citation)
type QuoteBlockPrms = () :: Row Type

type MathBlockOpts = (citation :: Maybe Citation)
type MathBlockPrms = (value :: String)

type ImageOpts = (citation :: Maybe Citation)
type ImagePrms = (url :: String)

type ErrorOpts = () :: Row Type
type ErrorPrms = (label :: String)

--------------------------------------------------------------------------------
-- types associated with Doc
--------------------------------------------------------------------------------

newtype RefId = RefId String

derive instance Newtype RefId _
derive newtype instance Show RefId
derive newtype instance Eq RefId
derive newtype instance Ord RefId
derive newtype instance EncodeJson RefId
derive newtype instance DecodeJson RefId

type StringStyle = Variant
  ( emphasis :: U
  , code :: U
  )

data Citation = Citation (Record CitationOpts) (Record CitationPrms)

type CitationOpts =
  ( time :: Maybe String
  , note :: Maybe String
  )

type CitationPrms =
  ( id :: String
  , index :: Int
  , resources :: Array Resource
  )

derive instance Generic Citation _

instance Show Citation where
  show x = genericShow x

instance EncodeJson Citation where
  encodeJson x = genericEncodeJson x

instance DecodeJson Citation where
  decodeJson x = genericDecodeJson x

citation :: forall r r'. Union r CitationOpts r' => Nub r' CitationOpts => Record r -> Record CitationPrms -> Citation
citation opts prms = Citation (opts `R.merge` { time: Nothing @String, note: Nothing @String }) prms

data Resource = Resource (Record ResourceOpts) (Record ResourcePrms)

type ResourceOpts =
  ( content :: Maybe ResourceContent
  )

type ResourceContent = Variant
  ( url :: String
  , misc :: String
  )

type ResourcePrms =
  ( name :: String
  )

derive instance Generic Resource _

instance Show Resource where
  show x = genericShow x

-- Resources are identified by their name
instance Eq Resource where
  eq (Resource _ { name: x1 }) (Resource _ { name: x2 }) = x1 == x2

instance Ord Resource where
  compare (Resource _ { name: x1 }) (Resource _ { name: x2 }) = compare x1 x2

instance EncodeJson Resource where
  encodeJson x = genericEncodeJson x

instance DecodeJson Resource where
  decodeJson x = genericDecodeJson x

resource :: forall r r'. Union r ResourceOpts r' => Nub r' ResourceOpts => Record r -> String -> Resource
resource opts name = Resource (opts `R.merge` { content: Nothing @ResourceContent }) { name }

--------------------------------------------------------------------------------
-- Doc builders
--------------------------------------------------------------------------------

page :: Record PageOpts -> Record PagePrms -> Array Doc -> Doc
page opts prms body = Fix.wrap $ Page opts prms body

section :: Record SectionOpts -> Record SectionPrms -> Array Doc -> Doc
section opts prms body = Fix.wrap $ Section opts prms body

paragraph :: Record ParagraphOpts -> Record ParagraphPrms -> Array Doc -> Doc
paragraph opts prms body = Fix.wrap $ Paragraph opts prms body

sentence :: Record SentenceOpts -> Record SentencePrms -> Array Doc -> Doc
sentence opts prms body = Fix.wrap $ Sentence opts prms body

linkExternal :: Record LinkExternalOpts -> Record LinkExternalPrms -> Doc -> Doc
linkExternal opts prms label = Fix.wrap $ LinkExternal (opts `R.merge` { favicon_url: Nothing @String, citation: Nothing @Citation, url: Nothing @String }) prms label

linkInternal :: Record LinkInternalOpts -> Record LinkInternalPrms -> Doc -> Doc
linkInternal opts prms label = Fix.wrap $ LinkInternal (opts `R.merge` { citation: Nothing @Citation, refId: Nothing @RefId }) prms label

sidenote :: Record SidenoteOpts -> Record SidenotePrms -> Doc -> Doc -> Doc
sidenote opts prms label body = Fix.wrap $ Sidenote opts prms label body

ref :: Record RefOpts -> Record RefPrms -> Doc
ref opts prms = Fix.wrap $ Ref opts prms

string :: Record StringOpts -> Record StringPrms -> Doc
string opts prms = Fix.wrap $ String opts prms

codeBlock :: Record CodeBlockOpts -> Record CodeBlockPrms -> Doc
codeBlock opts prms = Fix.wrap $ CodeBlock opts prms

quoteBlock :: Record QuoteBlockOpts -> Record QuoteBlockPrms -> Doc -> Doc
quoteBlock opts prms body = Fix.wrap $ QuoteBlock opts prms body

mathBlock :: Record MathBlockOpts -> Record MathBlockPrms -> Doc
mathBlock opts prms = Fix.wrap $ MathBlock opts prms

image :: Record ImageOpts -> Record ImagePrms -> Maybe Doc -> Doc
image opts prms caption = Fix.wrap $ Image opts prms caption

-- where
-- opts_input :: Record (ImageOpts_ (caption :: Maybe Doc))
-- opts_input = opts `R.merge` { citation: Nothing @Citation, caption: Nothing @Doc }

-- opts_output :: Record (ImageOpts_ ())
-- opts_output = R.delete (Proxy @"caption") opts_input

error :: Record ErrorOpts -> Record ErrorPrms -> Doc -> Doc
error opts prms body = Fix.wrap $ Error opts prms body

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
        xs <- ArrayST.new
        doc0 # Fix.traverse_upwards_ case _ of
          Ref _ args -> xs # ArrayST.push args.refId # void
          _ -> pure unit
        pure xs
    )
    # Set.fromFoldable

collectCitations :: Doc -> Array Citation
collectCitations doc0 =
  ArrayST.run
    ( do
        xs <- ArrayST.new
        let
          pushMaybe = maybe (pure unit) \cit -> xs # ArrayST.push cit # void
        doc0 # Fix.traverse_upwards_ case _ of
          LinkExternal opts _ _ -> opts.citation # pushMaybe
          LinkInternal opts _ _ -> opts.citation # pushMaybe
          CodeBlock opts _ -> opts.citation # pushMaybe
          QuoteBlock opts _ _ -> opts.citation # pushMaybe
          MathBlock opts _ -> opts.citation # pushMaybe
          Image opts _ _ -> opts.citation # pushMaybe
          _ -> pure unit
        pure xs
    )

