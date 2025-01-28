module Rybl.Language.Component.Basic where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (class MonadState, evalState, get, put)
import Control.Monad.Writer (tell)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut.Decode (fromJsonString)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Lens ((%=), (.=))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch as Fetch
import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen (ComponentHTML, liftAff)
import Halogen.HTML (div) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rybl.Data.Variant (case_, expandCons, inj', inj'U, on')
import Rybl.Halogen.Class as Class
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc, Ref)
import Rybl.Language.Component.Common (Env, Input, State)
import Rybl.Language.Component.Doc.Compact as Rybl.Language.Component.Doc.Compact
import Rybl.Utility (prop', todo)

--------------------------------------------------------------------------------
-- theDocComponent
--------------------------------------------------------------------------------

theDocComponent :: forall query output. H.Component query Input output Aff
theDocComponent = H.mkComponent { initialState, eval, render }
  where
  initialState { doc, viewMode } =
    { doc
    , viewMode
    , ctx:
        { namedDocs: Map.empty
        , display: inj'U @"block"
        , mb_target_sidenote_id: none
        }
    , env:
        { widgetIndex: 0 }
    } :: State

  eval = H.mkEval H.defaultEval
    { receive = pure <<< inj' @"receive"
    , initialize = pure $ inj'U @"initialize"
    , handleAction = handleAction
    }

  handleAction = case_
    # on' @"receive"
        ( \input -> do
            put $ initialState input
            handleAction (inj' @"initialize" unit)
        )
    # on' @"initialize"
        ( const do
            do -- load namedDocs
              let
                go :: Map Ref _ -> Set Ref -> Aff (Map Ref _)
                go namedDocs refs = case refs # Set.findMin of
                  Nothing -> pure namedDocs
                  Just ref -> do
                    response <-
                      fetch ("namedDocs/" <> ref <> ".json")
                        { method: Fetch.GET }
                    if not response.ok then do
                      let
                        v = inj' @"error_on_load" $
                          HH.div
                            []
                            [ HH.text $ "error on fetch: " <> response.statusText ]
                      go
                        (namedDocs # Map.insert ref v)
                        (refs # Set.delete ref)
                    else do
                      str :: String <- response.text # liftAff
                      case fromJsonString str :: JsonDecodeError \/ Doc of
                        Left err -> do
                          let
                            v = inj' @"error_on_load" $
                              HH.div
                                []
                                [ HH.text $ "error on decode: " <> show err ]
                          go
                            (namedDocs # Map.insert ref v)
                            (refs # Set.delete ref)
                        Right doc' -> do
                          let v = inj' @"loaded" doc'
                          go
                            (namedDocs # Map.insert ref v)
                            (todo "(refs # Set.union (doc' # collectRefs) # Set.delete ref)")
              { doc } <- get
              namedDocs' <- go Map.empty (doc # todo "collectRefs") # liftAff
              prop' @"ctx" <<< prop' @"namedDocs" .= namedDocs'
        )
    # on' @"modify_env" (prop' @"env" %= _)
    # on' @"modify_ctx" (prop' @"ctx" %= _)

  render { doc, ctx, env, viewMode: _ } =
    H.div
      [ HP.classes [ Class.mk @"doc" ]
      , Style.style $ tell [ "width: 100%" ]
      ]
      [ Rybl.Language.Component.Doc.Compact.renderDoc doc
          # flip runReaderT ctx
          # flip evalState env
      ]
      # mapAction_ComponentHTML (expandCons @"receive" >>> expandCons @"initialize")

nextSlotIndex :: forall m. MonadState Env m => m Int
nextSlotIndex = do
  { widgetIndex } <- get
  prop' @"widgetIndex" %= (_ + 1)
  pure widgetIndex

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

mapAction_ComponentHTML
  :: forall action action' slots m
   . (action -> action')
  -> ComponentHTML action slots m
  -> ComponentHTML action' slots m
mapAction_ComponentHTML f = bimap (map f) f

