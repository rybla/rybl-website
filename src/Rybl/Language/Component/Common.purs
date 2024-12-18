module Rybl.Language.Component.Common where

import Prelude

import Data.Const (Const)
import Data.Map (Map)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Rybl.Data.Variant (Variant, case_, on')
import Rybl.Language (Doc, ViewMode)
import Rybl.Utility (Literal(..), U, (##))

type Input =
  { doc :: Doc
  , viewMode :: ViewMode
  }

type State =
  { doc :: Doc
  , viewMode :: ViewMode
  , ctx :: Ctx
  , env :: Env
  }

type Ctx =
  { namedDocs ::
      Map String
        ( Variant
            ( loaded :: Doc
            , not_yet_loaded :: Unit
            , error_on_load :: HTML
            )
        )
  , display :: Display
  }

type Display = Literal ("block" :: U, "inline-block" :: U, "inline" :: U)

renderDisplayStyle :: Display -> String
renderDisplayStyle d = "display: " <> show d <> "; "

renderDisplayStyle_flex :: Display -> String
renderDisplayStyle_flex (Literal d) = d ## case_
  # on' @"block" (const "display: flex; ")
  # on' @"inline-block" (const "display: inline-flex; ")
  # on' @"inline" (const "display: inline-flex; ")

type Env =
  { widgetIndex :: Int
  }

type Action = Variant
  (
  )

type Slots :: Row Type
type Slots =
  ( widget :: H.Slot (Const Void) Void Int
  )

type M = Aff

type HTML = ComponentHTML Action Slots M

