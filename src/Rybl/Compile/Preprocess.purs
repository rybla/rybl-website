module Rybl.Compile.Preprocess where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Rybl.Data.Variant (inj', match)
import Rybl.Language (Doc(..))
import Rybl.Utility (bug, todo)
import Web.HTML (Location)
import Web.URL as URL

preprocessDoc :: Doc -> Aff Doc
preprocessDoc (Section doc) = do
  body <- doc.body # traverse preprocessDoc
  pure $ Section doc { body = body }
preprocessDoc (Paragraph doc) = do
  body <- doc.body # traverse preprocessDoc
  pure $ Paragraph doc { body = body }
preprocessDoc (Sentence doc) = do
  body <- doc.body # traverse preprocessDoc
  pure $ Sentence doc { body = body }
preprocessDoc (Link doc) = do
  label <- doc.label # preprocessDoc
  src <- doc.src # match
    { external: case _ of
        src | Nothing <- src.mb_favicon_src -> do
          let url = URL.fromAbsolute src.href # fromMaybe' (\_ -> bug $ "invalid href: " <> show src.href)
          let favicon_src = (url # URL.protocol) <> "//" <> (url # URL.hostname) <> "/" <> "favicon.ico"
          pure $ inj' @"external" src { mb_favicon_src = pure favicon_src }
        _ -> pure doc.src
    , internal: const $ pure doc.src
    }
  pure $ Link doc { label = label, src = src }
preprocessDoc (Sidenote doc) = do
  label <- doc.label # preprocessDoc
  body <- doc.body # preprocessDoc
  pure $ Sidenote doc { label = label, body = body }
preprocessDoc (Ref doc) = do
  pure $ Ref doc
preprocessDoc (String doc) = do
  pure $ String doc
preprocessDoc (Error doc) = do
  body <- doc.body # preprocessDoc
  pure $ Error doc { body = body }
