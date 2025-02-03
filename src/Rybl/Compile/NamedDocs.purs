module Rybl.Compile.NamedDocs where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Rybl.Language (Doc)
import Rybl.Language as Doc

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ Tuple "index" $
      Doc.ref (wrap "full_example_1")
  , Tuple "full_example_1" $
      Doc.section "Full Example #1"
        [ Doc.paragraph [ Doc.sentence [ Doc.string "This is a paragraph before a section." ] ]
        , Doc.section "Links"
            [ Doc.paragraph
                [ Doc.sentence
                    [ Doc.string "Here is a link to an external page: "
                    , Doc.link_external (Doc.string "google") { href: "https://www.google.com/", mb_favicon_src: pure "https://www.google.com/favicon.ico" }
                    , Doc.string "."
                    ]
                ]
            , Doc.paragraph
                [ Doc.sentence
                    [ Doc.string "And now here's a link to an internal page: "
                    , Doc.link_internal (Doc.string "lorem ipsum") { refId: wrap "lorem_ipsum" }
                    , Doc.string "."
                    ]
                ]
            ]
        , Doc.section "Sidenotes"
            [ Doc.paragraph
                [ Doc.sentence
                    [ Doc.string "I wanted to say a "
                    , Doc.sidenote
                        (Doc.string "thing")
                        (Doc.paragraph [ Doc.sentence [ Doc.string "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                    , Doc.string ", but turns out it's a little too long to say right here."
                    ]
                , Doc.sentence [ Doc.string "And this is another sentence." ]
                ]
            ]
        , Doc.section "Tree"
            ( make_section_tree 4 4
                [ Doc.paragraph [ Doc.sentence [ Doc.string "Test" ] ] ]
            )
        ]
  , Tuple "lorem_ipsum" $
      Doc.paragraph
        [ Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        , Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        ]
  , Tuple "sidenote_example_1" $
      Doc.section "Sidenote Example #1"
        [ Doc.paragraph
            [ Doc.sentence
                [ Doc.string "I wanted to say a "
                , Doc.sidenote
                    (Doc.string "thing")
                    (Doc.paragraph [ Doc.sentence [ Doc.string "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                , Doc.string ", but turns out it's a little too long to say right here."
                ]
            ]
        ]
  , Tuple "example_index_4"
      $ Doc.section "This is example_index_4 which is the title of the doc"
      $ fold
          [ [ Doc.paragraph
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
            ]
          , make_section_tree 4 4
              [ Doc.paragraph
                  [ Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                  , Doc.sentence [ Doc.string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , Doc.sentence [ Doc.string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                  ]
              ]
          ]

  ]

make_section_tree :: Int -> Int -> Array Doc -> Array Doc
make_section_tree depth breadth body =
  if depth == 0 then []
  else
    body <>
      Array.replicate breadth
        ( Doc.section "Section Title"
            (body <> make_section_tree (depth - 1) breadth body)
        )
