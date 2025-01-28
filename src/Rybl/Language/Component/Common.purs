module Rybl.Language.Component.Common where

import Prelude

import Data.Const (Const)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Rybl.Data.Variant (Variant, case_, on')
import Rybl.Language (Doc, Id)
import Rybl.Utility (U)

type ViewMode = Variant
  ( unknown :: U
  , mobile :: U
  , wide_desktop :: U
  , narrow_desktop :: U
  )

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
            , not_yet_loaded :: U
            , error_on_load :: PlainHTML
            )
        )
  , display :: Display
  , mb_target_sidenote_id :: Maybe Id
  }

type Display = Variant (block :: U, inline :: U)

renderDisplayStyle :: Display -> String
renderDisplayStyle = case_
  # on' @"block" (const "display: block; ")
  # on' @"inline" (const "display: inline; ")

renderDisplayStyle_flex :: Display -> String
renderDisplayStyle_flex = case_
  # on' @"block" (const "display: flex; ")
  # on' @"inline" (const "display: inline-flex; ")

type Env =
  { widgetIndex :: Int
  }

type Action = Variant
  ( modify_env :: Env -> Env
  , modify_ctx :: Ctx -> Ctx
  )

type Slots :: Row Type
type Slots =
  ( widget :: H.Slot (Const Void) Action Int
  )

type M = Aff

type HTML = ComponentHTML Action Slots M

