module Rybl.App where

import Prelude

import Data.Argonaut.Decode (fromJsonString)
import Data.Either (either)
import Data.Lens ((.=))
import Data.Maybe (Maybe(..))
import Rybl.Data.Variant (case_, inj', on')
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.VDom.Driver as HVD
import JSURI (decodeURI)
import Rybl.Language as Rybl.Language
import Rybl.Language.Component.Basic as Rybl.Language.Component
import Rybl.Utility (prop')
import Type.Proxy (Proxy(..))
import Web.HTML as Web.HTML
import Web.HTML.Event.PopStateEvent.EventTypes as Web.HTML.Event.PopStateEvent.EventTypes
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.Window as Web.HTML.Window
import Web.URL as Web.URL
import Web.URL.URLSearchParams as Web.URLSearchParams

main :: Effect Unit
main = HA.runHalogenAff $ HVD.runUI component {} =<< HA.awaitBody

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { doc: Rybl.Language.Ref "index" :: Rybl.Language.Doc
    , viewMode: inj' @"unknown" unit :: Rybl.Language.ViewMode
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ inj' @"initialize" unit
    , handleAction = handleAction
    }

  handleAction = case_
    # on' @"initialize"
        ( const do
            Console.log "[App.initialize]"
            -- initialize listener: popstate => update
            document <- Web.HTML.window >>= Web.HTML.Window.document # liftEffect
            void $ H.subscribe $
              HQE.eventListener
                Web.HTML.Event.PopStateEvent.EventTypes.popstate
                (document # Web.HTML.HTMLDocument.toEventTarget)
                (const $ pure $ inj' @"update" unit)
            -- first update
            handleAction (inj' @"update" unit)
        )
    # on' @"update"
        ( const do
            Console.log "[App.update]"
            -- update .doc
            mb_url <-
              Web.HTML.window
                >>= Web.HTML.Window.location
                >>= Web.HTML.Location.href
                >>= Web.URL.fromAbsolute >>> pure
                # liftEffect
            case mb_url of
              Nothing -> Console.log "no url"
              Just url -> do
                let urlSearchParams = url # Web.URL.searchParams
                case urlSearchParams # Web.URLSearchParams.get "doc" of
                  Nothing -> pure unit
                  Just doc_str_ -> do
                    doc <- case doc_str_ # decodeURI of
                      Nothing -> pure $ Rybl.Language.Error $ Rybl.Language.String $ "decodeURI error"
                      Just doc_str -> do
                        pure $
                          doc_str
                            # fromJsonString
                            # either (\err -> Rybl.Language.String $ "JsonDecodeError: " <> show err) identity
                    prop' @"doc" .= doc
            -- TODO: update .viewMode
            pure unit
        )

  render { doc, viewMode } =
    HH.div
      [ HP.style "margin: auto; max-width: 600px;" ]
      [ HH.slot_ (Proxy @"doc") unit Rybl.Language.Component.theDocComponent { doc, viewMode }
      ]

