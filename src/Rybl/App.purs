module Rybl.App where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD

main :: Effect Unit
main = HA.runHalogenAff $ HVD.runUI component {} =<< HA.awaitBody

component = H.mkComponent { initialState, eval, render }
  where
  initialState {} = {}
  eval = H.mkEval H.defaultEval
  render {} =
    HH.div
      []
      [ HH.text "<App/>" ]

