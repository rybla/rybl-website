module Rybl.Compile.Common where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.Either (Either(..), either, fromRight')
import Data.Either.Nested (type (\/))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Prim.Row (class Nub, class Union)
import Record as R
import Rybl.Language (CodeBlockOpts, CodeBlockPrms, Doc, ErrorOpts, ErrorPrms, ImageOpts_input, ImagePrms, LinkExternalOpts, LinkExternalPrms, LinkInternalOpts, LinkInternalPrms, MathBlockOpts, MathBlockPrms, PageOpts, PagePrms, ParagraphOpts, ParagraphPrms, QuoteBlockOpts, QuoteBlockPrms, RefId, RefOpts, RefPrms, SectionOpts, SectionPrms, SentenceOpts, SentencePrms, SidenoteOpts, SidenotePrms, StringOpts, StringPrms)
import Rybl.Language as RL
import Rybl.Utility (todo)

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

type Env = {}

initialEnv :: forall m. MonadAff m => m Env
initialEnv = pure {}

type Err = String

--------------------------------------------------------------------------------
-- utilities
--------------------------------------------------------------------------------

makeIdFromTitle :: forall m. MonadAff m => String -> M m String
makeIdFromTitle = todo ""

--------------------------------------------------------------------------------
-- basic Doc builders
--------------------------------------------------------------------------------

type PagePrmsRequired = (title :: String)

page :: forall opts opts' prms m. Union opts PageOpts opts' => Nub opts' PageOpts => Union PagePrmsRequired PagePrms prms => Nub prms PagePrms => MonadAff m => Record opts -> Record PagePrmsRequired -> M m (Array Doc) -> M m Doc
page opts prms body = do
  id <- makeIdFromTitle prms.title
  RL.page opts (prms `R.merge` { id }) <$> body

type SectionPrmsRequired = (title :: String)

section :: forall opts opts' prms m. Union opts SectionOpts opts' => Nub opts' SectionOpts => Union SectionPrmsRequired SectionPrms prms => Nub prms SectionPrms => MonadAff m => Record opts -> Record SectionPrmsRequired -> M m (Array Doc) -> M m Doc
section opts prms body = do
  id <- makeIdFromTitle prms.title
  RL.section opts (prms `R.merge` { id }) <$> body

type ParagraphPrmsRequired = () :: Row Type

paragraph :: forall opts opts' prms m. Union opts ParagraphOpts opts' => Nub opts' ParagraphOpts => Union ParagraphPrmsRequired ParagraphPrms prms => Nub prms ParagraphPrms => MonadAff m => Record opts -> Record ParagraphPrmsRequired -> M m (Array Doc) -> M m Doc
paragraph opts prms body = RL.paragraph opts prms <$> body

type SentencePrmsRequired = () :: Row Type

sentence :: forall opts opts' prms m. Union opts SentenceOpts opts' => Nub opts' SentenceOpts => Union SentencePrmsRequired SentencePrms prms => Nub prms SentencePrms => MonadAff m => Record opts -> Record SentencePrmsRequired -> M m (Array Doc) -> M m Doc
sentence opts prms body = RL.sentence opts prms <$> body

type LinkExternalPrmsRequired = (url :: String)

linkExternal :: forall opts opts' prms m. Union opts LinkExternalOpts opts' => Nub opts' LinkExternalOpts => Union LinkExternalPrmsRequired LinkExternalPrms prms => Nub prms LinkExternalPrms => MonadAff m => Record opts -> Record LinkExternalPrmsRequired -> M m Doc -> M m Doc
linkExternal opts prms label = RL.linkExternal opts prms <$> label

type LinkInternalPrmsRequired = (refId :: RefId)

linkInternal :: forall opts opts' prms m. Union opts LinkInternalOpts opts' => Nub opts' LinkInternalOpts => Union LinkInternalPrmsRequired LinkInternalPrms prms => Nub prms LinkInternalPrms => MonadAff m => Record opts -> Record LinkInternalPrmsRequired -> M m Doc -> M m Doc
linkInternal opts prms label = RL.linkInternal opts prms <$> label

type SidenotePrmsRequired = () :: Row Type

sidenote :: forall opts opts' prms m. Union opts SidenoteOpts opts' => Nub opts' SidenoteOpts => Union SidenotePrmsRequired SidenotePrms prms => Nub prms SidenotePrms => MonadAff m => Record opts -> Record SidenotePrmsRequired -> M m Doc -> M m Doc -> M m Doc
sidenote opts prms label body = RL.sidenote opts prms <$> label <*> body

type RefPrmsRequired = (refId :: RefId)

ref :: forall opts opts' prms m. Union opts RefOpts opts' => Nub opts' RefOpts => Union RefPrmsRequired RefPrms prms => Nub prms RefPrms => MonadAff m => Record opts -> Record RefPrmsRequired -> M m Doc
ref opts prms = RL.ref opts prms # pure

type StringPrmsRequired = (value :: String)

string :: forall opts opts' prms m. Union opts StringOpts opts' => Nub opts' StringOpts => Union StringPrmsRequired StringPrms prms => Nub prms StringPrms => MonadAff m => Record opts -> Record StringPrmsRequired -> M m Doc
string opts prms = RL.string opts prms # pure

type CodeBlockPrmsRequired = (value :: String)

codeBlock :: forall opts opts' prms m. Union opts CodeBlockOpts opts' => Nub opts' CodeBlockOpts => Union CodeBlockPrmsRequired CodeBlockPrms prms => Nub prms CodeBlockPrms => MonadAff m => Record opts -> Record CodeBlockPrmsRequired -> M m Doc
codeBlock opts prms = RL.codeBlock opts prms # pure

type QuoteBlockPrmsRequired = () :: Row Type

quoteBlock :: forall opts opts' prms m. Union opts QuoteBlockOpts opts' => Nub opts' QuoteBlockOpts => Union QuoteBlockPrmsRequired QuoteBlockPrms prms => Nub prms QuoteBlockPrms => MonadAff m => Record opts -> Record QuoteBlockPrmsRequired -> M m Doc -> M m Doc
quoteBlock opts prms body = RL.quoteBlock opts prms <$> body

type MathBlockPrmsRequired = (value :: String)

mathBlock :: forall opts opts' prms m. Union opts MathBlockOpts opts' => Nub opts' MathBlockOpts => Union MathBlockPrmsRequired MathBlockPrms prms => Nub prms MathBlockPrms => MonadAff m => Record opts -> Record MathBlockPrmsRequired -> M m Doc
mathBlock opts prms = RL.mathBlock opts prms # pure

type ImagePrmsRequired = (url :: String)

image :: forall opts opts' prms m. Union opts ImageOpts_input opts' => Nub opts' ImageOpts_input => Union ImagePrmsRequired ImagePrms prms => Nub prms ImagePrms => MonadAff m => Record opts -> Record ImagePrmsRequired -> M m Doc
image opts prms = RL.image opts prms # pure

type ErrorPrmsRequired = (label :: String)

error :: forall opts opts' prms m. Union opts ErrorOpts opts' => Nub opts' ErrorOpts => Union ErrorPrmsRequired ErrorPrms prms => Nub prms ErrorPrms => MonadAff m => Record opts -> Record ErrorPrmsRequired -> M m Doc -> M m Doc
error opts prms body = RL.error opts prms <$> body

--------------------------------------------------------------------------------
-- advanced Doc builders
--------------------------------------------------------------------------------

-- TODO