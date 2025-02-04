module Rybl.Compile.Preprocess where

import Prelude

import Control.Monad.State (StateT, evalStateT, get)
import Data.Lens ((+=))
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String as String
import Effect.Aff (Aff)
import JSURI (encodeURIComponent)
import Rybl.Data.Fix as Fix
import Rybl.Language (Doc, Doc_(..))
import Rybl.Utility (bug, impossible, prop', todo)
import Web.URL as URL

preprocessDoc :: Doc -> Aff Doc
preprocessDoc doc = preprocessDoc_ doc # flip evalStateT
  { section_id_counter: 0
  }

type Env =
  { section_id_counter :: Int
  }

preprocessDoc_ :: Doc -> StateT Env Aff Doc
preprocessDoc_ = Fix.foldM case _ of
  Section opts args body -> do
    { section_id_counter } <- get
    prop' @"section_id_counter" += 1
    -- give unique id
    id <- opts.id # case _ of
      Just id -> pure $ pure id
      Nothing -> pure $ pure $
        args.title
          # String.replace (String.Pattern " ") (String.Replacement "_")
          # (_ <> "__" <> show section_id_counter)
          # encodeURIComponent >>> fromMaybe' impossible
    pure $ Fix.wrap $ Section opts { id = id } args body
  ExternalLink opts args label -> do
    -- compute and query favicon
    favicon_url <- case opts.favicon_url of
      Just favicon_url -> pure favicon_url
      Nothing -> do
        let url = args.url # URL.fromAbsolute # fromMaybe' (\_ -> bug $ "invalid href: " <> show args.url)
        pure $ (url # URL.protocol) <> "//" <> (url # URL.hostname) <> "/" <> "favicon.ico"
    pure $ Fix.wrap $ ExternalLink opts { favicon_url = pure favicon_url } args label
  doc -> pure $ Fix.wrap doc
