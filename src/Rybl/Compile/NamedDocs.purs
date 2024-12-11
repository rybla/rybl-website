module Rybl.Compile.NamedDocs where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Rybl.Language (Doc(..))

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ "example_doc_1" /\
      String "This is example doc 1"
  , "example_doc_2" /\
      String "This is example doc 2"
  , "example_doc_3" /\
      String "This is example doc 3"
  , "example_doc_4" /\
      String "This is example doc 4"
  ]
