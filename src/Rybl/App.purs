module Rybl.App where

import Prelude

import Data.Variant (inj')
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Rybl.Language as Doc
import Rybl.Language.BasicUI (theDocComponent)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff $ HVD.runUI component {} =<< HA.awaitBody

component :: H.Component _ _ _ Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState {} = {}
  eval = H.mkEval H.defaultEval
  render {} =
    HH.div
      [ HP.style "margin: auto; max-width: 40em;" ]
      [ HH.slot_ (Proxy @"doc") unit theDocComponent
          { doc:
              Doc.Group (inj' @"column" unit)
                [ Doc.String "This is an example Doc that references a bunch of named documents that should be encoded as JSON files."
                , Doc.Ref "example_doc_1"
                , Doc.Ref "example_doc_2"
                , Doc.Ref "example_doc_3"
                , Doc.Ref "example_doc_4"
                , Doc.Ref "example_doc_5"
                , Doc.Expander (inj' @"block" unit)
                    (Doc.String "This is a label for an Expander")
                    ( Doc.Group (inj' @"column" unit)
                        [ Doc.Ref "lorem_ipsum_short"
                        , Doc.Expander (inj' @"block" unit)
                            (Doc.String "This is a label for an Expander")
                            (Doc.Ref "lorem_ipsum_long")
                        ]
                    )
                ]
          }
      ]

