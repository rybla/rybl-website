module Rybl.Compile.Common where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array as Array
import Data.Either (Either(..), either, fromRight')
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Prim.Row (class Cons, class Nub, class Union)
import Record as R
import Rybl.Language (CodeBlockOpts, CodeBlockPrms, Doc, ErrorOpts, ErrorPrms, ImagePrms, LinkExternalOpts, LinkExternalPrms, LinkInternalOpts, LinkInternalPrms, MathBlockOpts, MathBlockPrms, PageOpts, PagePrms, ParagraphOpts, ParagraphPrms, QuoteBlockOpts, QuoteBlockPrms, RefId, RefOpts, RefPrms, Resource(..), SectionOpts, SectionPrms, SentenceOpts, SentencePrms, SidenoteOpts, SidenotePrms, StringOpts, StringPrms, StringStyle, ImageOpts)
import Rybl.Language as RL
import Rybl.Utility (encodeURIComponent_nicely, todo)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type M m = ExceptT Err (ReaderT Ctx (StateT Env (m)))

runM :: forall m a. MonadAff m => String -> M m a -> m a
runM label ma = do
  ctx <- initialCtx
  env <- initialEnv
  err_a /\ _env' <- ma # runExceptT # flip runReaderT ctx # flip runStateT env
  a <- err_a # either (\err -> liftAff $ throwError $ Aff.error $ label <> ": " <> err) pure
  pure a

type Ctx = {}

initialCtx :: forall m. MonadAff m => m Ctx
initialCtx = pure {}

type Env =
  { ids :: Array String
  }

initialEnv :: forall m. MonadAff m => m Env
initialEnv = pure
  { ids: none
  }

type Err = String

--------------------------------------------------------------------------------
-- basic Doc builders
--------------------------------------------------------------------------------

type PagePrmsRequired = (title :: String)

page :: forall opts opts' prms m. Union opts PageOpts opts' => Nub opts' PageOpts => Union PagePrmsRequired PagePrms prms => Nub prms PagePrms => MonadAff m => Record opts -> Record PagePrmsRequired -> M m (Array Doc) -> M m Doc
page opts prms body = do
  id <- makeIdFromTitle prms.title
  RL.page (opts `R.merge` {}) (prms `R.merge` { id }) <$> body

type SectionPrmsRequired = (title :: String)

section :: forall opts opts' prms m. Union opts SectionOpts opts' => Nub opts' SectionOpts => Union SectionPrmsRequired SectionPrms prms => Nub prms SectionPrms => MonadAff m => Record opts -> Record SectionPrmsRequired -> M m (Array Doc) -> M m Doc
section opts prms body = do
  id <- makeIdFromTitle prms.title
  RL.section (opts `R.merge` {}) (prms `R.merge` { id }) <$> body

type ParagraphPrmsRequired = () :: Row Type

paragraph :: forall opts opts' prms m. Union opts ParagraphOpts opts' => Nub opts' ParagraphOpts => Union ParagraphPrmsRequired ParagraphPrms prms => Nub prms ParagraphPrms => MonadAff m => Record opts -> Record ParagraphPrmsRequired -> M m (Array Doc) -> M m Doc
paragraph opts prms body = RL.paragraph (opts `R.merge` {}) prms <$> body

type SentencePrmsRequired = () :: Row Type

sentence :: forall opts opts' prms m. Union opts SentenceOpts opts' => Nub opts' SentenceOpts => Union SentencePrmsRequired SentencePrms prms => Nub prms SentencePrms => MonadAff m => Record opts -> Record SentencePrmsRequired -> M m (Array Doc) -> M m Doc
sentence opts prms body = RL.sentence (opts `R.merge` {}) prms <$> body

type LinkExternalPrmsRequired = () :: Row Type

linkExternal :: forall opts opts' prms m. Union opts LinkExternalOpts opts' => Nub opts' LinkExternalOpts => Union LinkExternalPrmsRequired LinkExternalPrms prms => Nub prms LinkExternalPrms => MonadAff m => Record opts -> Record LinkExternalPrmsRequired -> M m Doc -> M m Doc
linkExternal opts prms label = RL.linkExternal (opts `R.merge` { favicon_url: Nothing @String, source: Nothing @Resource, url: Nothing @String }) prms <$> label

-- linkExternal
--   :: forall opts opts' opts_tmp opts_tmp_ prms m
--    . Union opts LinkExternalOpts opts'
--   => Nub opts' LinkExternalOpts
--   => Union opts (url :: Maybe String, favicon_url :: Maybe String) opts_tmp
--   => Nub opts_tmp (url :: Maybe String, favicon_url :: Maybe String | opts_tmp_)
--   => Union LinkExternalPrmsRequired LinkExternalPrms prms
--   => Nub prms LinkExternalPrms
--   => MonadAff m
--   => Record opts
--   -> Record LinkExternalPrmsRequired
--   -> M m Doc
--   -> M m Doc
-- linkExternal opts prms label = do
--   -- -- TODO: if url provided, then look for favicon
--   -- let
--   --   opts_tmp_ :: Record (url :: Maybe String, favicon_url :: Maybe String | opts_tmp_)
--   --   opts_tmp_ = opts `R.merge` { url: Nothing @String, favicon_url: Nothing @String }
--   -- favicon_url <- case opts_tmp_.url /\ opts_tmp_.favicon_url of
--   --   Nothing /\ _ -> pure Nothing
--   --   _ /\ Just favicon_url -> pure $ Just favicon_url
--   --   Just url /\ Nothing -> ?a
--   -- RL.linkExternal opts prms <$> label
--   todo ""
type LinkInternalPrmsRequired = () :: Row Type

linkInternal :: forall opts opts' prms m. Union opts LinkInternalOpts opts' => Nub opts' LinkInternalOpts => Union LinkInternalPrmsRequired LinkInternalPrms prms => Nub prms LinkInternalPrms => MonadAff m => Record opts -> Record LinkInternalPrmsRequired -> M m Doc -> M m Doc
linkInternal opts prms label = RL.linkInternal (opts `R.merge` { source: Nothing @Resource, refId: Nothing @RefId }) prms <$> label

type SidenotePrmsRequired = () :: Row Type

sidenote :: forall opts opts' prms m. Union opts SidenoteOpts opts' => Nub opts' SidenoteOpts => Union SidenotePrmsRequired SidenotePrms prms => Nub prms SidenotePrms => MonadAff m => Record opts -> Record SidenotePrmsRequired -> M m Doc -> M m Doc -> M m Doc
sidenote opts prms label body = RL.sidenote (opts `R.merge` {}) prms <$> label <*> body

type RefPrmsRequired = (refId :: RefId)

ref :: forall opts opts' prms m. Union opts RefOpts opts' => Nub opts' RefOpts => Union RefPrmsRequired RefPrms prms => Nub prms RefPrms => MonadAff m => Record opts -> Record RefPrmsRequired -> M m Doc
ref opts prms = RL.ref (opts `R.merge` {}) prms # pure

type StringPrmsRequired = (value :: String)

string :: forall opts opts' prms m. Union opts StringOpts opts' => Nub opts' StringOpts => Union StringPrmsRequired StringPrms prms => Nub prms StringPrms => MonadAff m => Record opts -> Record StringPrmsRequired -> M m Doc
string opts prms = RL.string (opts `R.merge` { style: Nothing @StringStyle }) prms # pure

type CodeBlockPrmsRequired = (value :: String)

codeBlock :: forall opts opts' prms m. Union opts CodeBlockOpts opts' => Nub opts' CodeBlockOpts => Union CodeBlockPrmsRequired CodeBlockPrms prms => Nub prms CodeBlockPrms => MonadAff m => Record opts -> Record CodeBlockPrmsRequired -> M m Doc
codeBlock opts prms = RL.codeBlock (opts `R.merge` { source: Nothing @Resource }) prms # pure

type QuoteBlockPrmsRequired = () :: Row Type

quoteBlock :: forall opts opts' prms m. Union opts QuoteBlockOpts opts' => Nub opts' QuoteBlockOpts => Union QuoteBlockPrmsRequired QuoteBlockPrms prms => Nub prms QuoteBlockPrms => MonadAff m => Record opts -> Record QuoteBlockPrmsRequired -> M m Doc -> M m Doc
quoteBlock opts prms body = RL.quoteBlock (opts `R.merge` { source: Nothing @Resource }) prms <$> body

type MathBlockPrmsRequired = (value :: String)

mathBlock :: forall opts opts' prms m. Union opts MathBlockOpts opts' => Nub opts' MathBlockOpts => Union MathBlockPrmsRequired MathBlockPrms prms => Nub prms MathBlockPrms => MonadAff m => Record opts -> Record MathBlockPrmsRequired -> M m Doc
mathBlock opts prms = RL.mathBlock (opts `R.merge` { source: Nothing @Resource }) prms # pure

type ImagePrmsRequired = (url :: String)

image :: forall opts opts' prms m. Union opts ImageOpts opts' => Nub opts' ImageOpts => Union ImagePrmsRequired ImagePrms prms => Nub prms ImagePrms => MonadAff m => Record opts -> Record ImagePrmsRequired -> M m (Maybe Doc) -> M m Doc
image opts prms caption = RL.image (opts `R.merge` { source: Nothing @Resource }) prms <$> caption

type ErrorPrmsRequired = (label :: String)

error :: forall opts opts' prms m. Union opts ErrorOpts opts' => Nub opts' ErrorOpts => Union ErrorPrmsRequired ErrorPrms prms => Nub prms ErrorPrms => MonadAff m => Record opts -> Record ErrorPrmsRequired -> M m Doc -> M m Doc
error opts prms body = RL.error (opts `R.merge` {}) prms <$> body

--------------------------------------------------------------------------------
-- advanced Doc builders
--------------------------------------------------------------------------------

-- TODO

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

makeIdFromTitle :: forall m. MonadAff m => String -> M m String
makeIdFromTitle title = do
  let id = encodeURIComponent_nicely title
  { ids } <- get
  let n_overlaps = ids # Array.filter (_ == id) # Array.length
  modify_ _ { ids = ids `Array.snoc` id }
  if n_overlaps > 0 then
    pure $ title <> "__" <> id
  else
    pure title
