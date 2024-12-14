module Rybl.Language.Component.Common where

import Prelude

import Data.Const (Const)
import Data.Map (Map)
import Rybl.Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Rybl.Language (Doc, ViewMode)

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
  }

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

