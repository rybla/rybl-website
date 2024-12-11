module Rybl.Language.Common where

import Prelude

import Data.Map (Map)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen (ComponentHTML)
import Rybl.Language (Doc)

type Input =
  { doc :: Doc
  }

type State =
  { doc :: Doc
  , ctx :: Ctx
  }

type Ctx =
  { namedDocs ::
      Map String
        ( Variant
            ( loaded :: Doc
            , not_yet_loaded :: {}
            , error_on_load :: HTML
            )
        )
  }

type Action = Variant
  ( receive :: Input
  , initialize :: {}
  )

type Slots :: Row Type
type Slots = ()

type M = Aff

type HTML = ComponentHTML Action Slots M