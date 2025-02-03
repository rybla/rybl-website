module Rybl.Compile.Preprocess where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (wrap)
import Effect.Aff (Aff)
import Rybl.Data.Fix as Fix
import Rybl.Data.Variant (inj', match)
import Rybl.Language (Doc, Doc_(..))
import Rybl.Utility (bug, todo)
import Web.URL as URL

preprocessDoc :: Doc -> Aff Doc
preprocessDoc = Fix.foldM case _ of
  Link doc label -> do
    src <- doc.src # match
      { external: case _ of
          src | Nothing <- src.mb_favicon_src -> do
            let url = URL.fromAbsolute src.href # fromMaybe' (\_ -> bug $ "invalid href: " <> show src.href)
            let favicon_src = (url # URL.protocol) <> "//" <> (url # URL.hostname) <> "/" <> "favicon.ico"
            pure $ inj' @"external" src { mb_favicon_src = pure favicon_src }
          _ -> pure doc.src
      , internal: const $ pure doc.src
      }
    pure $ Fix.wrap $ Link doc { src = src } label
  doc -> pure $ Fix.wrap doc

-- preprocessDoc :: Doc -> Aff Doc
-- preprocessDoc = todo ""