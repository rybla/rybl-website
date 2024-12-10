module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD
import Rybl.App as App

main :: Effect Unit
main = HA.runHalogenAff $ HVD.runUI App.component {} =<< HA.awaitBody
