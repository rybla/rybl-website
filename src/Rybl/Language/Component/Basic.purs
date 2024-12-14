module Rybl.Language.Component.Basic where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, evalState, get)
import Control.Monad.State.Class (put)
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
import Data.Traversable (traverse)
import Rybl.Data.Variant (case_, expandCons, inj', on')
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch as Fetch
import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen (ComponentHTML, liftAff)
import Halogen.HTML (div) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Rybl.Language (Doc(..), Ref, ExpanderStyle, collectRefs)
import Rybl.Language.Component.Common (Ctx, Env, Input, State, HTML)
import Rybl.Utility (prop', (##))
import Type.Prelude (Proxy(..))

theDocComponent :: forall query output. H.Component query Input output Aff
theDocComponent = H.mkComponent { initialState, eval, render }
  where
  initialState { doc, viewMode } =
    { doc
    , viewMode
    , ctx:
        { namedDocs: Map.empty }
    , env:
        { widgetIndex: 0 }
    } :: State

  eval = H.mkEval H.defaultEval
    { receive = pure <<< inj' @"receive"
    , initialize = pure $ inj' @"initialize" unit
    , handleAction = case_
        # on' @"receive" (put <<< initialState)
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
                                (refs # Set.union (doc' # collectRefs) # Set.delete ref)
                  { doc } <- get
                  namedDocs' <- go Map.empty (doc # collectRefs) # liftAff
                  prop' @"ctx" <<< prop' @"namedDocs" .= namedDocs'
            )
    }

  render { doc, ctx, env } =
    H.div
      []
      [ renderDoc doc
          # flip runReaderT ctx
          # flip evalState env
      ]
      # mapAction_ComponentHTML (expandCons @"receive" >>> expandCons @"initialize")

renderDoc :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML
renderDoc (String str) = do
  pure
    $ HH.div
        []
        [ HH.text str ]

renderDoc (Error d) = do
  e <- d # renderDoc
  pure
    $ HH.div
        [ HP.style "background-color: #ffcccb;" ]
        [ e ]

renderDoc (Group sty ds) = do
  kids <- ds # traverse renderDoc
  let
    gap = "0.5em"
    style flexDirection = "display: flex; flex-direction: " <> flexDirection <> "; gap: " <> gap <> ";"
  sty ## case_
    # on' @"column"
        ( const do
            pure
              $ HH.div
                  [ HP.style $ style "column" ]
                  kids

        )
    # on' @"row"
        ( const do
            pure
              $ HH.div
                  [ HP.style $ style "row" ]
                  kids

        )

renderDoc (Ref x) = do
  { namedDocs } <- ask
  case namedDocs # Map.lookup x of
    Nothing -> pure
      $ HH.div
          []
          [ HH.text $ "missing ref: " <> show x ]
    Just a -> a ## case_
      # on' @"loaded" renderDoc
      # on' @"not_yet_loaded"
          ( const $ pure
              $ HH.div
                  []
                  [ HH.text $ "loading ref " <> show x ]

          )
      # on' @"error_on_load"
          ( \err -> pure
              $ HH.div
                  [ HP.style "display: flex; flex-direction: column; background-color: #ffcccb;" ]
                  [ HH.text $ "error when loading ref " <> show x
                  , err
                  ]
          )

renderDoc (Expander sty label_ body_) = do
  widgetIndex <- nextSlotIndex
  label <- renderDoc label_
  body <- renderDoc body_
  pure $ HH.slot_ (Proxy @"widget") widgetIndex theExpanderComponent { sty, label, body }

nextSlotIndex :: forall m. MonadState Env m => m Int
nextSlotIndex = do
  { widgetIndex } <- get
  prop' @"widgetIndex" %= (_ + 1)
  pure widgetIndex

theExpanderComponent :: forall query output. H.Component query { sty :: ExpanderStyle, label :: HTML, body :: HTML } output Aff
theExpanderComponent = H.mkComponent { initialState, eval, render }
  where
  initialState { sty, label, body } =
    { sty, label, body, is_open: false }

  eval = H.mkEval H.defaultEval
    { receive = pure <<< inj' @"receive"
    , handleAction = case_
        # on' @"receive" (put <<< initialState)
        # on' @"toggle_is_open" (const (prop' @"is_open" %= not))
    }

  render { sty, label, body, is_open } =
    HH.div
      [ HP.style "box-shadow: 0 0 0 0.1em black inset; display: flex; flex-direction: column; padding: 0.5em; gap: 0.5em;" ]
      [ label # mapAction_ComponentHTML (expandCons @"toggle_is_open")
      , HH.button
          [ HE.onClick $ const $ inj' @"toggle_is_open" unit ]
          [ HH.text "expand" ]
      , if is_open then
          body # mapAction_ComponentHTML (expandCons @"toggle_is_open")
        else
          HH.div [] [ HH.text "..." ]
      ]
      # mapAction_ComponentHTML (expandCons @"receive")

mapAction_ComponentHTML
  :: forall action action' slots m
   . (action -> action')
  -> ComponentHTML action slots m
  -> ComponentHTML action' slots m
mapAction_ComponentHTML f = bimap (map f) f

