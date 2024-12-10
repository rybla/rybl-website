module Rybl.Language.BasicUI where

import Prelude

import Halogen as H
import Rybl.Language (Doc)

-- component = H.mkComponent { initialState, eval, render }
--   where
--   initialState { doc } = { doc: doc :: Doc }
--   eval = H.mkEval H.defaultEval
--   render { doc } =
--     H.div
--       []
--       [ "<"]