module Rybl.Language.Component.Basic where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (class MonadState, evalState, get, modify_, put)
import Control.Monad.Writer (tell)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Lens ((%=))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch as Fetch
import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen (liftAff)
import Halogen.HTML (div) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rybl.Data.Fix as Fix
import Rybl.Data.Variant (case_, expandCons, inj', inj'U, on')
import Rybl.Halogen.Class as Class
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc, Doc_(..), RefId, collectRefIds)
import Rybl.Language as RL
import Rybl.Language.Component.Common (Env, Input, State, mapAction_ComponentHTML)
import Rybl.Language.Component.Doc.Compact as Compact
import Rybl.Language.Component.Doc.Compact as Rybl.Language.Component.Doc.Compact
import Rybl.Utility (prop')

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
        { namedDocs: Map.empty :: Map RefId _
        , section_path: mempty
        }
    , env:
        { widget_index: 0
        , section_index: 0
        }
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
            -- load namedDocs
            do
              let
                loadRefIds :: Map RefId Doc -> Set RefId -> Aff (Map RefId Doc)
                loadRefIds namedDocs refIds = case refIds # Set.findMin of
                  Nothing -> pure namedDocs
                  Just refId -> do
                    response <-
                      fetch ("namedDocs/" <> unwrap refId <> ".json")
                        { method: Fetch.GET }
                    if not response.ok then do
                      loadRefIds
                        (namedDocs # Map.insert refId (RL.error {} "error on fetch" (RL.string { style: inj'U @"code" # pure } response.statusText)))
                        (refIds # Set.delete refId)
                    else do
                      str :: String <- response.text # liftAff
                      case fromJsonString str :: JsonDecodeError \/ Doc of
                        Left err -> do
                          loadRefIds
                            (namedDocs # Map.insert refId (RL.error {} "error on decode" (RL.string { style: inj'U @"code" # pure } (show err))))
                            (refIds # Set.delete refId)
                        Right doc' -> do
                          loadRefIds
                            (namedDocs # Map.insert refId doc')
                            (refIds # Set.union (doc' # collectRefIds) # Set.delete refId)

                expandRefs :: Map RefId Doc -> Doc -> Aff Doc
                expandRefs namedDocs = Fix.foldM case _ of
                  Ref _opts args -> case namedDocs # Map.lookup args.refId of
                    Nothing -> pure $ RL.error {} "refId not loaded" (RL.string { style: inj'U @"code" # pure } (show args.refId))
                    Just doc -> pure doc
                  doc -> pure $ Fix.wrap doc

                fixpoint :: Map RefId Doc -> Doc -> Aff (Map RefId Doc /\ Doc)
                fixpoint namedDocs doc = do
                  namedDocs' <- loadRefIds Map.empty (doc # collectRefIds) # liftAff
                  if (namedDocs # Map.keys) == (namedDocs' # Map.keys) then
                    pure (namedDocs /\ doc)
                  else do
                    doc' <- expandRefs namedDocs' doc
                    fixpoint namedDocs' doc'

              { doc } <- get
              namedDocs' /\ doc' <- fixpoint Map.empty doc # liftAff
              modify_ _ { doc = doc', ctx { namedDocs = namedDocs' } }
        )
    # on' @"modify_env" (prop' @"env" %= _)
    # on' @"modify_ctx" (prop' @"ctx" %= _)

  render { doc, ctx, env, viewMode: _ } =
    H.div
      [ Style.style $ tell [ "width: 100%" ] ]
      ( ( do
            htmls_doc <- doc # Compact.renderDoc
            htmls_toc <- doc # Compact.renderTableOfContents
            pure $ fold
              [ [ HH.div
                    [ Style.style do tell [ "padding: 0.5em", "border: 1px solid black" ] ]
                    [ htmls_toc ]
                ]
              , htmls_doc
              ]
        )
          # flip runReaderT ctx
          # flip evalState env
      )
      # mapAction_ComponentHTML (expandCons @"receive" >>> expandCons @"initialize")

nextSlotIndex :: forall m. MonadState Env m => m Int
nextSlotIndex = do
  { widget_index } <- get
  prop' @"widget_index" %= (_ + 1)
  pure widget_index
