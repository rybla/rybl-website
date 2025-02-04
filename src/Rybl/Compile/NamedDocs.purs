module Rybl.Compile.NamedDocs where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (wrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Rybl.Data.Variant (inj', inj'U)
import Rybl.Language (Doc, codeBlock, external_link, image, internal_link, mathBlock, page, paragraph, quoteBlock, ref, section, sentence, sidenote, string)

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ Tuple "index" $
      ref {} (wrap "full_example_1")
  , Tuple "full_example_1" $
      page {} "Full Example #1"
        [ section {} "String Styles"
            [ paragraph {}
                [ sentence {}
                    [ string {} "This is an emphasized string: "
                    , string { style: inj'U @"emphasis" # pure } "roar"
                    ]
                ]
            , paragraph {}
                [ sentence {}
                    [ string {} "This is a code string: "
                    , string { style: inj'U @"code" # pure } "x&&2 and one Class123 + $"
                    ]
                ]
            ]
        , section {} "Links"
            [ paragraph {}
                [ sentence {}
                    [ string {} "Here is a link to an external page: "
                    , external_link {} (string {} "google") "https://www.google.com/"
                    , string {} "."
                    ]
                ]
            , paragraph {}
                [ sentence {}
                    [ string {} "And now here's a link to an internal page: "
                    , internal_link {} (string {} "lorem ipsum") (wrap "lorem_ipsum")
                    , string {} "."
                    ]
                ]
            ]
        , section {} "Sidenotes"
            [ paragraph {}
                [ sentence {}
                    [ string {} "I wanted to say a "
                    , sidenote {}
                        (string {} "thing")
                        (paragraph {} [ sentence {} [ string {} "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                    , string {} ", but turns out it's a little too long to say right here."
                    ]
                , sentence {} [ string {} "And this is another sentence {}." ]
                ]
            ]
        , section {} "Codeblocks"
            [ paragraph {}
                [ sentence {} [ string {} "The following is a pretty narrow code block." ] ]
            , codeBlock {} $
                """
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
""" # String.trim
            , paragraph {}
                [ sentence {} [ string {} "The following is a very wise code block." ] ]
            , codeBlock {} $
                """
big_list = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
""" # String.trim
            ]
        , section {} "Quoteblocks"
            [ paragraph {}
                [ sentence {} [ string {} "The following is a quote block." ] ]
            , quoteBlock {} $
                paragraph {} [ string {} "I certainly believe this: that it is better to be impetuous than cautious, because Fortune is a woman, and if you want to keep her under it is necessary to beat her and force her down. It is clear that she more often allows herself to be won over by impetuous men than by those who proceed coldly. And so, like a woman, Fortune is always the friend of young men, for they are less cautious, more ferocious, and command her with more audacity." ]
            ]
        , section {} "Mathblocks"
            [ paragraph {}
                [ sentence {} [ string {} "The following is a math block." ] ]
            , mathBlock {} $
                """
f(x) = \lim_{h \to 0} \frac{A(x+h) - A(x)}{h}
""" # String.trim
            ]
        , section {} "Media"
            [ paragraph {} [ string {} "The following is an image." ]
            , image
                { caption: pure $ string {} "This is an image of a cute cat. Isn't it so cute? Really, it is."
                , source: pure $ { name: pure "The Cat Pic", date: pure "Today", source: pure $ inj' @"url" "https://media.4-paws.org/9/c/9/7/9c97c38666efa11b79d94619cc1db56e8c43d430/Molly_006-2829x1886-2726x1886-1920x1328.jpg" }
                }
                "https://media.4-paws.org/9/c/9/7/9c97c38666efa11b79d94619cc1db56e8c43d430/Molly_006-2829x1886-2726x1886-1920x1328.jpg"
            ]
        -- , section {} "Tree"
        --     ( make_section_tree 4 4
        --         [ paragraph {} [ sentence {} [ string {} "Test" ] ] ]
        --     )
        ]
  , Tuple "lorem_ipsum" $
      paragraph {}
        [ sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        , sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
        , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
        ]
  , Tuple "sidenote_example_1" $
      section {} "Sidenote Example #1"
        [ paragraph {}
            [ sentence {}
                [ string {} "I wanted to say a "
                , sidenote {}
                    (string {} "thing")
                    (paragraph {} [ sentence {} [ string {} "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." ] ])
                , string {} ", but turns out it's a little too long to say right here."
                ]
            ]
        ]
  , Tuple "example_index_4"
      $ section {} "This is example_index_4 which is the title of the doc"
      $ fold
          [ [ paragraph {}
                [ sentence {}
                    [ internal_link {} (string {} "This") (wrap "example_index_3")
                    , string {} " is a ref to the example_index_3."
                    ]
                , sentence {} [ string {} "Isn't that great?" ]
                , sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                , sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                ]
            ]
          , make_section_tree 4 4
              [ paragraph {}
                  [ sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
                  , sentence {} [ string {} "Lorem ipsum dolor sit amet consectetur adipisicing elit." ]
                  , sentence {} [ string {} "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" ]
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
        ( section {} "Section Title"
            (body <> make_section_tree (depth - 1) breadth body)
        )
