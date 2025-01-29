module Rybl.Compile.NamedDocs where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Rybl.Language (Doc)
import Rybl.Language as Doc

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ Tuple "index" $
      Doc.ref (wrap "example_index_4")
  , Tuple "example_index_4"
      $ Doc.section
          (Doc.string "This is example_index_4 which is the title of the doc")
      $
        [ Doc.paragraph
            [ Doc.sentence
                [ Doc.link_internal (Doc.string "This") { refId: wrap "example_index_3" }
                , Doc.string " is a ref to the example_index_3."
                ]
            , Doc.sentence [ Doc.string "Isn't that great?" ]
            , Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
            , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
            , Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
            , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
            ]
        ] <>
          make_section_tree 4 4
            [ Doc.paragraph
                [ Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                , Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                ]
            ]
  ]

make_section_tree :: Int -> Int -> Array Doc -> Array Doc
make_section_tree depth breadth body =
  if depth == 0 then []
  else
    body <>
      Array.replicate breadth
        ( Doc.section
            (Doc.string "Section Title")
            (body <> make_section_tree (depth - 1) breadth body)
        )
