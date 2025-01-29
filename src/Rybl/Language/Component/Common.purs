module Rybl.Language.Component.Common where

import Prelude

import Data.Const (Const)
import Data.List (List)
import Data.Map (Map)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Rybl.Data.Variant (Variant)
import Rybl.Language (Doc, RefId)
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
      Map RefId
        ( Variant
            ( loaded :: Doc
            , not_yet_loaded :: U
            , error_on_load :: PlainHTML
            )
        )
  , section_path :: List { index :: Int, title :: Doc }
  }

type Env =
  { widget_index :: Int
  , section_index :: Int
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

