module Rybl.App where

import Prelude

import Control.Applicative (pure)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (either)
import Data.Lens ((.=))
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (case_, inj', on')
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.VDom.Driver as HVD
import JSURI (decodeURI, encodeURI)
import Rybl.Language as Doc
import Rybl.Language.BasicUI (theDocComponent)
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

component :: H.Component _ _ _ Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState {} =
    { doc: Doc.Ref "index"
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ inj' @"initialize" unit
    , handleAction = handleAction
    }

  handleAction = case_
    # on' @"initialize"
        ( const do
            document <- Web.HTML.window >>= Web.HTML.Window.document # liftEffect
            void $ H.subscribe $
              HQE.eventListener
                Web.HTML.Event.PopStateEvent.EventTypes.popstate
                (document # Web.HTML.HTMLDocument.toEventTarget)
                (const $ pure $ inj' @"update" unit)
            handleAction (inj' @"update" unit)
            pure unit
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
                case urlSearchParams # Web.URLSearchParams.get "doc" of
                  Nothing -> pure unit
                  Just doc_str_ -> do
                    doc <- case doc_str_ # decodeURI of
                      Nothing -> pure $ Doc.Error $ Doc.String $ "decodeURI error"
                      Just doc_str -> do
                        pure $
                          doc_str
                            # fromJsonString
                            # either (\err -> Doc.String $ "JsonDecodeError: " <> show err) identity
                    prop' @"doc" .= doc
        )

  render { doc } =
    HH.div
      [ HP.style "margin: auto; max-width: 40em;" ]
      [ HH.slot_ (Proxy @"doc") unit theDocComponent
          { doc:
              --   Doc.Group (inj' @"column" unit)
              --     [ Doc.String "This is an example Doc that references a bunch of named documents that should be encoded as JSON files."
              --     , Doc.Ref "example_doc_1"
              --     , Doc.Ref "example_doc_2"
              --     , Doc.Ref "example_doc_3"
              --     , Doc.Ref "example_doc_4"
              --     , Doc.Ref "example_doc_5"
              --     , Doc.Expander (inj' @"block" unit)
              --         (Doc.String "This is a label for an Expander")
              --         ( Doc.Group (inj' @"column" unit)
              --             [ Doc.Ref "lorem_ipsum_short"
              --             , Doc.Expander (inj' @"block" unit)
              --                 (Doc.String "This is a label for an Expander")
              --                 (Doc.Ref "lorem_ipsum_long")
              --             ]
              --         )
              --     ]
              doc
          }
      ]

