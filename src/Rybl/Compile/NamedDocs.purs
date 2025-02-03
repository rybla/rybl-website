module Rybl.Compile.NamedDocs where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Rybl.Data.Variant (inj')
import Rybl.Language (Doc, link_external, link_internal, paragraph, ref, section, sentence, sidenote, string, string_style)

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ Tuple "index" $
      ref (wrap "full_example_1")
  , Tuple "full_example_1" $
      section "Full Example #1"
        [ section "String Styles"
            [ paragraph
                [ sentence
                    [ string "This is an emphasized string: "
                    , string_style (inj' @"emphasis" unit) "roar"
                    ]
                ]
            , paragraph
                [ sentence
                    [ string "This is a code string: "
                    , string_style (inj' @"code" unit) "x&&2 and one Class123 + $"
                    ]
                ]
            ]
        , section "Links"
            [ paragraph
                [ sentence
                    [ string "Here is a link to an external page: "
                    , link_external (string "google") { href: "https://www.google.com/", mb_favicon_src: none }
                    , string "."
                    ]
                ]
            , paragraph
                [ sentence
                    [ string "And now here's a link to an internal page: "
                    , link_internal (string "lorem ipsum") { refId: wrap "lorem_ipsum" }
                    , string "."
                    ]
                ]
            ]
        , section "Sidenotes"
            [ paragraph
                [ sentence
                    [ string "I wanted to say a "
                    , sidenote
                        (string "thing")
                        (paragraph [ sentence [ string "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                    , string ", but turns out it's a little too long to say right here."
                    ]
                , sentence [ string "And this is another sentence." ]
                ]
            ]
        , section "Tree"
            ( make_section_tree 4 4
                [ paragraph [ sentence [ string "Test" ] ] ]
            )
        ]
  , Tuple "lorem_ipsum" $
      paragraph
        [ sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        , sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        ]
  , Tuple "sidenote_example_1" $
      section "Sidenote Example #1"
        [ paragraph
            [ sentence
                [ string "I wanted to say a "
                , sidenote
                    (string "thing")
                    (paragraph [ sentence [ string "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                , string ", but turns out it's a little too long to say right here."
                ]
            ]
        ]
  , Tuple "example_index_4"
      $ section "This is example_index_4 which is the title of the doc"
      $ fold
          [ [ paragraph
                [ sentence
                    [ link_internal (string "This") { refId: wrap "example_index_3" }
                    , string " is a ref to the example_index_3."
                    ]
                , sentence [ string "Isn't that great?" ]
                , sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                , sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                ]
            ]
          , make_section_tree 4 4
              [ paragraph
                  [ sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                  , sentence [ string "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , sentence [ string "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
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
        ( section "Section Title"
            (body <> make_section_tree (depth - 1) breadth body)
        )
