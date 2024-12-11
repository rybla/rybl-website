module Rybl.Language.BasicUI where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, runReader)
import Control.Monad.State (get, put)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Lens ((.=))
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, inj', on')
import Data.Variant as V
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch as Fetch
import Halogen (Component, defaultEval, mkComponent, mkEval) as H
import Halogen (liftAff)
import Halogen.HTML (div) as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Rybl.Language (Doc(..), collectRefs)
import Rybl.Language.Common (HTML, Input, State, Ctx)
import Rybl.Utility (prop', (##))

theDocComponent :: H.Component _ Input _ Aff
theDocComponent = H.mkComponent { initialState, eval, render }
  where
  initialState { doc } =
    { doc
    , ctx:
        { namedDocs:
            doc
              # collectRefs
              # Set.toUnfoldable
              # map @List (\x -> x /\ inj' @"not_yet_loaded" {})
              # Map.fromFoldable
        }
    } :: State

  eval = H.mkEval H.defaultEval
    { receive = pure <<< inj' @"receive"
    , initialize = pure $ inj' @"initialize" {}
    , handleAction = case_
        # on' @"receive" (put <<< initialState)
        # on' @"initialize"
            ( const do
                -- load namedDocs
                do
                  { ctx: { namedDocs } } <- get
                  namedDocs' <- namedDocs # traverseWithIndex \x -> case_
                    # const (pure <<< V.expand)
                    # on' @"not_yet_loaded"
                        ( const do
                            response <-
                              fetch ("namedDocs/" <> x <> ".json")
                                { method: Fetch.GET
                                } # liftAff
                            if not response.ok then do
                              pure $ inj' @"error_on_load" $
                                HH.div
                                  []
                                  [ HH.text $ "error on fetch: " <> response.statusText ]
                            else do
                              str :: String <- response.text # liftAff
                              case fromJsonString str :: JsonDecodeError \/ Doc of
                                Left err -> pure $ inj' @"error_on_load" $
                                  HH.div
                                    []
                                    [ HH.text $ "error on decode: " <> show err ]
                                Right d -> pure $ inj' @"loaded" d
                        )
                  prop' @"ctx" <<< prop' @"namedDocs" .= namedDocs'
            )
    }

  render { doc, ctx } =
    H.div
      []
      [ renderDoc doc
          # flip runReader ctx
      ]

renderDoc :: forall m. MonadReader Ctx m => Doc -> m HTML
renderDoc (String str) = do
  pure
    $ HH.span
        []
        [ HH.text str ]

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
                  [ HP.style "display: flex; flex-direction: column; box-shadow: 0 0 0 0.1em red inset; padding: 0.5em" ]
                  [ HH.text $ "error when loading ref " <> show x
                  , err
                  ]
          )
