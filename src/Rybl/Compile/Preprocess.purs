module Rybl.Compile.Preprocess where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Effect.Aff (Aff)
import Rybl.Data.Fix as Fix
import Rybl.Language (Doc, Doc_(..))
import Rybl.Utility (bug)
import Web.URL as URL

preprocessDoc :: Doc -> Aff Doc
preprocessDoc = Fix.foldM case _ of
  ExternalLink opts args label -> do
    favicon_url <- case opts.favicon_url of
      Just favicon_url -> pure favicon_url
      Nothing -> do
        let url = args.url # URL.fromAbsolute # fromMaybe' (\_ -> bug $ "invalid href: " <> show args.url)
        pure $ (url # URL.protocol) <> "//" <> (url # URL.hostname) <> "/" <> "favicon.ico"
    pure $ Fix.wrap $ ExternalLink opts { favicon_url = pure favicon_url } args label
  doc -> pure $ Fix.wrap doc
