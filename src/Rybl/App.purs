module Rybl.App where

import Prelude

import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.Lens ((.=))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Unfoldable (none)
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
import Rybl.Data.Variant (case_, inj', inj'U, on')
import Rybl.Halogen.Class as Class
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc)
import Rybl.Language as RL
import Rybl.Language.Component.Basic as RC
import Rybl.Language.Component.Common (ViewMode)
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
    { doc: RL.ref {} { refId: wrap "index" } :: Doc
    , viewMode: inj' @"unknown" unit :: ViewMode
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
                case unit of
                  _ | Just ref_str_ <- urlSearchParams # Web.URLSearchParams.get "ref" -> do
                    doc <- case ref_str_ # decodeURI of
                      Nothing ->
                        pure
                          $ RL.error {} { label: "decodeURI error" }
                          $ RL.string { style: inj'U @"code" # pure }
                              { value: "ref_str = " <> "\"\"\"" <> ref_str_ <> "\"\"\"" }
                      Just ref_str -> pure $ RL.ref {} { refId: wrap ref_str }
                    prop' @"doc" .= doc
                  _ | Just doc_str_ <- urlSearchParams # Web.URLSearchParams.get "doc" -> do
                    doc <- case doc_str_ # decodeURI of
                      Nothing -> pure
                        $ RL.error {} { label: "decodeURI error" }
                        $ RL.string { style: inj'U @"code" # pure }
                            { value: "doc_str = " <> "\"\"\"" <> doc_str_ <> "\"\"\"" }

                      Just doc_str -> do
                        pure $
                          doc_str
                            # fromJsonString
                            # either
                                ( \err ->
                                    RL.error {} { label: "JsonDecodeError" }
                                      ( RL.section {} { title: "Errors", id: "errors", path: List.fromFoldable [ "errors" ] }
                                          [ RL.string { style: inj'U @"code" # pure }
                                              { value: intercalate "\n" [ "doc_str = " <> "\"\"\"" <> doc_str <> "\"\"\"" ] }
                                          , RL.string { style: inj'U @"code" # pure }
                                              { value: intercalate "\n" [ "err = " <> "\"\"\"" <> printJsonDecodeError err <> "\"\"\"" ] }
                                          ]
                                      )
                                )
                                identity
                    prop' @"doc" .= doc
                  _ -> pure unit
            -- TODO: update .viewMode
            pure unit
        )

  render { doc, viewMode } =
    HH.div
      [ HP.classes [ Class.mk @"app" ]
      , Style.css $ tell [ "margin: auto", "max-width: 800px", "display: flex", "flex-direction: column", "gap: 0.5rem" ]
      ]
      -- [ HH.div
      --     [ Style.css $ tell [ "font-size: 2rem", "padding: 0 1rem" ] ]
      --     [ HH.text "rybl" ]
      -- , HH.div
      --     [ Style.css $ tell [ "padding: 0 1rem", "display: flex", "flex-flow: row wrap", "gap: 0.5rem", "line-height: 1.5" ] ]
      --     let
      --       item_style = tell [ "padding: 0.5em", "border: 1px solid black", "width: 4em" ]
      --     in
      --       [ HH.div [ Style.css item_style ] [ HH.text "index" ]
      --       , HH.div [ Style.css item_style ] [ HH.text "about" ]
      --       , HH.div [ Style.css item_style ] [ HH.text "links" ]
      --       , HH.div [ Style.css item_style ] [ HH.text "contact" ]
      --       ]
      -- , HH.div
      --     [ Style.css $ tell [ "margin-top: 0.5rem", "padding: 0.5rem", "border: 0.5rem solid black", "box-shadow: 0 0 1rem 0 black" ] ]
      --     [ HH.slot_ (Proxy @"doc") unit RyblComponent.theDocComponent { doc, viewMode } ]
      -- ]
      [ HH.div
          [ Style.css $ tell [ "margin-top: 0.5rem", "padding: 0.5rem" ] ]
          [ HH.slot_ (Proxy @"doc") unit RC.theDocComponent { doc, viewMode } ]
      ]

