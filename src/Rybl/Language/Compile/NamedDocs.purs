module Rybl.Language.Compile.NamedDocs where

import Prelude
import Rybl.Language.Compile.Common

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Rybl.Data.Variant (inj', inj'U)
import Rybl.Language (Doc, RefId(..), resource)

namedDocs :: Aff (Map RefId Doc)
namedDocs =
  sequence $ Map.fromFoldable
    [ item "index" do ref {} { refId: wrap "demo" }
    , item "demo" do
        page {} { title: "Demo" } $ sequence $
          [ section {} { title: "String Styles" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "This is an emphasized string: " }
                      , string { style: inj'U @"emphasis" # pure } { value: "roar" }
                      ]
                  ]
              , paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "This is a code string: " }
                      , string { style: inj'U @"code" # pure } { value: "x&&2 and one Class123 + $" }
                      ]
                  ]
              ]
          , section {} { title: "Links" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "Notice that links are annotated with the favicon of their source website." } ]
                  , sentence {} {} $ sequence $
                      [ string {} { value: "Here is a link to an external page: " }
                      , linkExternal { url: "https://www.google.com/" # pure } {} $ string {} { value: "google" }
                      , string {} { value: "." }
                      ]
                  , sentence {} {} $ sequence $
                      [ string {} { value: "Here is a nub link to an external page: " }
                      , linkExternal {} {} $ string {} { value: "cool mars website" }
                      , string {} { value: "." }
                      ]
                  ]
              , paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "And now here's a link to an internal page: " }
                      , linkInternal { refId: wrap "lorem_ipsum" # pure } {} $ string {} { value: "lorem ipsum" }
                      , string {} { value: "." }
                      ]
                  , sentence {} {} $ sequence $
                      [ string {} { value: "And also here's a nub link to an internal page: " }
                      , linkInternal {} {} $ string {} { value: "the most awesom stuff" }
                      , string {} { value: "." }
                      ]
                  ]
              ]
          , section {} { title: "Sidenotes" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "I wanted to say a " }
                      , sidenote {} {}
                          (string {} { value: "thing" })
                          ( paragraph {} {} $ sequence $
                              [ sentence {} {} $ sequence $
                                  [ string {} { value: "This is the longer thing that I wanted to say, because it didn't quite fit in that tiny little space that was provided." } ]
                              ]
                          )
                      , string {} { value: ", but turns out it's a little too long to say right here." }
                      ]
                  , sentence {} {} $ sequence $
                      [ string {} { value: "And this is another sentence that is stretching out a bit in order to show what it looks like when the above sidenote is expanded with some text beneath it, that is taking up space that will be moved around during the expansion." } ]
                  ]
              ]
          , section {} { title: "Codeblocks" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $ [ string {} { value: "The following is a pretty narrow code block." } ] ]
              , codeBlock { source: resource { content: inj' @"url" "https://lesharmoniesdelesprit.wordpress.com/wp-content/uploads/2015/11/whiteheadrussell-principiamathematicavolumei.pdf" # pure } "Principia Mathematica" # pure }
                  { value:
                      """
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
""" # String.trim
                  }

              , paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "The following is a very wise code block." } ]
                  ]
              , codeBlock {}
                  { value:
                      """
big_list = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3]
""" # String.trim
                  }
              ]
          , section {} { title: "Quoteblocks" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "The following is a quote block." } ]
                  ]
              , quoteBlock { source: pure $ resource {} "Albert Einstein" } {} $ paragraph {} {} $ sequence $
                  [ string {} { value: "I certainly believe this: that it is better to be impetuous than cautious, because Fortune is a woman, and if you want to keep her under it is necessary to beat her and force her down. It is clear that she more often allows herself to be won over by impetuous men than by those who proceed coldly. And so, like a woman, Fortune is always the friend of young men, for they are less cautious, more ferocious, and command her with more audacity." } ]
              ]
          , section {} { title: "Mathblocks" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ sentence {} {} $ sequence $
                      [ string {} { value: "The following is a math block." } ]
                  ]
              , mathBlock {}
                  { value:
                      """
f(x) = \lim_{h \to 0} \frac{A(x+h) - A(x)}{h}
""" # String.trim
                  }
              ]
          , section {} { title: "Media" } $ sequence $
              [ paragraph {} {} $ sequence $
                  [ string {} { value: "The following is an image." } ]
              , image
                  { source: resource { date: pure "Today", content: pure $ inj' @"url" "https://media.4-paws.org/9/c/9/7/9c97c38666efa11b79d94619cc1db56e8c43d430/Molly_006-2829x1886-2726x1886-1920x1328.jpg" } "The Cat Pic" # pure }
                  { url: "https://media.4-paws.org/9/c/9/7/9c97c38666efa11b79d94619cc1db56e8c43d430/Molly_006-2829x1886-2726x1886-1920x1328.jpg" }
                  $ sequence
                  $ Just (string {} { value: "This is an image of a cute cat. Isn't it so cute? Really, it is." })
              ]
          ]
    , item "lorem_ipsum" do
        paragraph {} {} $ sequence $
          [ sentence {} {} $ sequence $ [ string {} { value: "Lorem ipsum dolor sit amet consectetur adipisicing elit." } ]
          , sentence {} {} $ sequence $ [ string {} { value: "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" } ]
          , sentence {} {} $ sequence $ [ string {} { value: "Lorem ipsum dolor sit amet consectetur adipisicing elit." } ]
          , sentence {} {} $ sequence $ [ string {} { value: "Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!" } ]
          ]
    , item "big_tree" do
        page {} { title: "Big Tree" }
          $ makeSectionTree 4 4
          $ sequence
              [ ref {} { refId: wrap "lorem_ipsum" } ]
    , item "small_tree" do
        page {} { title: "Small Tree" }
          $ makeSectionTree 2 2
          $ sequence
              [ ref {} { refId: wrap "lorem_ipsum" } ]
    , item "parse_example_1" do
        -- (section {} { title: "Page" } $ sequence $ [ (section {} { title: "Subtitle 1" } $ sequence $ [ (section {} { title: "Subtitle 1.1" } $ sequence $ [ (section {} { title: "Subtitle 2" } $ sequence $ []) ]) ]) ])
        (section {} { title: "Page" } $ sequence $ [ (section {} { title: "Subtitle 1" } $ sequence $ [ (section {} { title: "Subtitle 1.1" } $ sequence $ []) ]), (section {} { title: "Subtitle 2" } $ sequence $ []) ])
    ]
  where
  item str m = Tuple (RefId str) (runM str m)

makeSectionTree :: forall m. MonadAff m => Int -> Int -> M m (Array Doc) -> M m (Array Doc)
makeSectionTree depth breadth body = do
  body' <- body
  let
    go 0 = pure body'
    go n = do
      rest <- section {} { title: "Section Title" } (go (n - 1))
      pure $ body' <> Array.replicate breadth rest
  go depth
