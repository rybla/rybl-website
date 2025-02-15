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
import Type.Prelude (Proxy(..))

newtype RefId = RefId String

derive instance Newtype RefId _
derive newtype instance Show RefId
derive newtype instance Eq RefId
derive newtype instance Ord RefId
derive newtype instance EncodeJson RefId
derive newtype instance DecodeJson RefId

type Doc = Fix Doc_

data Doc_ self
  = Page (Record PageOpts) { title :: String } (Array self)
  | Section (Record SectionOpts) { title :: String } (Array self)
  | Paragraph (Record ParagraphOpts) {} (Array self)
  | Sentence (Record SentenceOpts) {} (Array self)
  | ExternalLink (Record LinkExternalOpts) { url :: String } self
  | InternalLink (Record LinkInternalOpts) { refId :: RefId } self
  | Sidenote (Record SidenoteOpts) {} self self
  | Ref (Record RefOpts) { refId :: RefId }
  | String (Record StringOpts) { value :: String }
  | CodeBlock (Record CodeBlockOpts) { value :: String }
  | QuoteBlock (Record QuoteBlockOpts) {} self
  | MathBlock (Record MathBlockOpts) { value :: String }
  -- TODO: other media (videos/models/etc)
  | Image (Record ImageOpts) { url :: String } (Maybe Doc)
  | Error (Record ErrorOpts) { label :: String } self

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
  ( emphasis :: U
  , code :: U
  )

data Resource = Resource (Record ResourceOpts) { name :: String }

type ResourceOpts =
  ( date :: Maybe String
  , content :: Maybe ResourceContent
  )

type ResourceContent = Variant
  ( url :: String
  , misc :: String
  )

derive instance Generic Resource _

instance Show Resource where
  show x = genericShow x

instance EncodeJson Resource where
  encodeJson x = genericEncodeJson x

instance DecodeJson Resource where
  decodeJson x = genericDecodeJson x

resource :: forall r r'. Union r ResourceOpts r' => Nub r' ResourceOpts => Record r -> String -> Resource
resource opts name = Resource (opts `R.merge` { date: Nothing @String, content: Nothing @ResourceContent }) { name }

--------------------------------------------------------------------------------
-- Doc builders
--------------------------------------------------------------------------------

type PageOpts = (id :: Maybe String) :: Row Type

page :: forall r r'. Union r PageOpts r' => Nub r' PageOpts => Record r -> String -> Array Doc -> Doc
page opts title body = Fix.wrap $ Page (opts `R.merge` { id: Nothing @String }) { title } body

type SectionOpts = (id :: Maybe String) :: Row Type

section :: forall r r'. Union r SectionOpts r' => Nub r' SectionOpts => Record r -> String -> Array Doc -> Doc
section opts title body = Fix.wrap $ Section (opts `R.merge` { id: Nothing @String }) { title } body

type ParagraphOpts = () :: Row Type

paragraph :: forall r r'. Union r ParagraphOpts r' => Nub r' ParagraphOpts => Record r -> Array Doc -> Doc
paragraph opts body = Fix.wrap $ Paragraph (opts `R.merge` {}) {} body

type SentenceOpts = () :: Row Type

sentence :: forall r r'. Union r SentenceOpts r' => Nub r' SentenceOpts => Record r -> Array Doc -> Doc
sentence opts body = Fix.wrap $ Sentence (opts `R.merge` {}) {} body

type LinkExternalOpts = (favicon_url :: Maybe String, source :: Maybe Resource)

external_link :: forall r r'. Union r LinkExternalOpts r' => Nub r' LinkExternalOpts => Record r -> Doc -> String -> Doc
external_link opts label url = Fix.wrap $ ExternalLink (opts `R.merge` { favicon_url: Nothing @String, source: Nothing @Resource }) { url } label

type LinkInternalOpts = (source :: Maybe Resource)

internal_link :: forall r r'. Union r LinkInternalOpts r' => Nub r' LinkInternalOpts => Record r -> Doc -> RefId -> Doc
internal_link opts label refId = Fix.wrap $ InternalLink (opts `R.merge` { source: Nothing @Resource }) { refId } label

type SidenoteOpts = () :: Row Type

sidenote :: forall r r'. Union r SidenoteOpts r' => Nub r' SidenoteOpts => Record r -> Doc -> Doc -> Doc
sidenote opts label body = Fix.wrap $ Sidenote (opts `R.merge` {}) {} label body

type RefOpts = () :: Row Type

ref :: forall r r'. Union r RefOpts r' => Nub r' RefOpts => Record r -> RefId -> Doc
ref opts refId = Fix.wrap $ Ref (opts `R.merge` {}) { refId }

type StringOpts = (style :: Maybe StringStyle)

string :: forall r r'. Union r StringOpts r' => Nub r' StringOpts => Record r -> String -> Doc
string opts value = Fix.wrap $ String (opts `R.merge` { style: Nothing @StringStyle }) { value }

type CodeBlockOpts = (source :: Maybe Resource)

codeBlock :: forall r r'. Union r CodeBlockOpts r' => Nub r' CodeBlockOpts => Record r -> String -> Doc
codeBlock opts value = Fix.wrap $ CodeBlock (opts `R.merge` { source: Nothing @Resource }) { value }

type QuoteBlockOpts = (source :: Maybe Resource)

quoteBlock :: forall r r'. Union r QuoteBlockOpts r' => Nub r' QuoteBlockOpts => Record r -> Doc -> Doc
quoteBlock opts body = Fix.wrap $ QuoteBlock (R.merge opts { source: Nothing @Resource }) {} body

type MathBlockOpts = (source :: Maybe Resource)

mathBlock :: forall r r'. Union r MathBlockOpts r' => Nub r' MathBlockOpts => Record r -> String -> Doc
mathBlock opts value = Fix.wrap $ MathBlock (opts `R.merge` { source: Nothing @Resource }) { value }

type ImageOpts_ r = (source :: Maybe Resource | r)
type ImageOpts = ImageOpts_ ()
type ImageOpts_input = ImageOpts_ (caption :: Maybe Doc)

image :: forall r r'. Union r ImageOpts_input r' => Nub r' ImageOpts_input => Record r -> String -> Fix Doc_
image opts url = Fix.wrap $ Image opts_output { url } opts_input.caption
  where
  opts_input :: Record (ImageOpts_ (caption :: Maybe Doc))
  opts_input = opts `R.merge` { source: Nothing @Resource, caption: Nothing @Doc }

  opts_output :: Record (ImageOpts_ ())
  opts_output = R.delete (Proxy @"caption") opts_input

type ErrorOpts = () :: Row Type

error :: forall r r'. Union r ErrorOpts r' => Nub r' ErrorOpts => Record r -> String -> Doc -> Doc
error opts label body = Fix.wrap $ Error (opts `R.merge` {}) { label } body

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

collectResources :: Doc -> Array Resource
collectResources doc0 =
  ArrayST.run
    ( do
        xs <- ArrayST.new
        let
          pushMaybeResource = maybe (pure unit) \source -> xs # ArrayST.push source # void
        doc0 # Fix.traverse_upwards_ case _ of
          ExternalLink opts _ _ -> opts.source # pushMaybeResource
          InternalLink opts _ _ -> opts.source # pushMaybeResource
          CodeBlock opts _ -> opts.source # pushMaybeResource
          QuoteBlock opts _ _ -> opts.source # pushMaybeResource
          MathBlock opts _ -> opts.source # pushMaybeResource
          Image opts _ _ -> opts.source # pushMaybeResource
          _ -> pure unit
        pure xs
    )

