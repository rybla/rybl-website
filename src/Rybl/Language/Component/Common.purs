module Rybl.Language.Component.Common where

import Prelude

import Control.Monad.State (class MonadState, get)
import Data.Bifunctor (bimap)
import Data.Const (Const)
import Data.Lens ((+=))
import Data.List (List)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Rybl.Data.Variant (Variant)
import Rybl.Language (Doc, RefId)
import Rybl.Utility (U, prop')

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
  { namedDocs :: Map RefId Doc
  , section_path :: List { index :: Int, title :: String }
  }

type WidgetIndex = Int

type Env =
  { widget_index :: WidgetIndex
  , section_index :: Int
  }

next_widget_index :: forall m. MonadState Env m => m Int
next_widget_index = do
  { widget_index } <- get
  prop' @"widget_index" += 1
  pure widget_index

type Action = Variant
  ( modify_env :: Env -> Env
  , modify_ctx :: Ctx -> Ctx
  )

type Slots :: Row Type
type Slots =
  ( widget :: H.Slot (Const Void) Action WidgetIndex
  , "SidenoteExpander" :: H.Slot (Const Void) Action WidgetIndex
  )

type M = Aff

type HTML = ComponentHTML Action Slots M

mapAction_ComponentHTML
  :: forall action action' slots m
   . (action -> action')
  -> ComponentHTML action slots m
  -> ComponentHTML action' slots m
mapAction_ComponentHTML f = bimap (map f) f

