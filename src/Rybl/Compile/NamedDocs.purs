module Rybl.Compile.NamedDocs where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Rybl.Data.Variant (inj')
import Rybl.Language (Doc(..))

namedDocs :: Map String Doc
namedDocs = Map.fromFoldable
  [ "index" /\ Ref "example_index_2"
  , "example_index_2" /\
      Group (inj' @"column" unit)
        [ SidenotesThreshold
            ( Group (inj' @"flow" unit)
                [ String "This is an"
                , Sidenote "1"
                    (String "example")
                    (String "This is an example sidenote body. Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!")
                , String "of a sidenote. And, Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
                ]
            )
        , String "Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
        , String "Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
        , String "Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
        , SidenotesThreshold
            ( Group (inj' @"flow" unit)
                [ String "this is an"
                , Sidenote "1"
                    (String "example")
                    (String "this is an example sidenote body. Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!")
                , String "of a sidenote. and Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
                ]
            )
        , String "Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
        , SidenotesThreshold
            ( Group (inj' @"flow" unit)
                [ String "this is an"
                , Sidenote "1"
                    (String "example")
                    (String "this is an example sidenote body. Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!")
                , String "of a sidenote. and Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi! Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!"
                , String "And then here is another "
                , Sidenote "2"
                    (String "example")
                    (String "this is an example sidenote body. Lorem ipsum dolor sit amet consectetur adipisicing elit. Iure facilis, consequuntur necessitatibus aliquid ex nemo quos dolore, dicta ea possimus ratione cupiditate magni, saepe nulla odio odit aperiam incidunt eligendi!")
                , String "for your pleasure"
                ]
            )
        ]
  , "example_index_1" /\
      Group (inj' @"column" unit)
        [ String "This is an example Doc that references a bunch of named documents that should be encoded as JSON files."
        , Ref "example_doc_1"
        , Ref "example_doc_2"
        , Ref "example_doc_3"
        , Ref "example_doc_4"
        , Ref "example_doc_5"
        , Expander (inj' @"block" unit)
            (String "This is a label for an Expander")
            ( Group (inj' @"column" unit)
                [ Ref "lorem_ipsum_short"
                , Expander (inj' @"block" unit)
                    (String "This is a label for an Expander")
                    (Ref "lorem_ipsum_long")
                ]
            )
        ]
  , "example_doc_1" /\
      String "This is example doc 1"
  , "example_doc_2" /\
      String "This is example doc 2"
  , "example_doc_3" /\
      String "This is example doc 3"
  , "example_doc_4" /\
      String "This is example doc 4"
  , "lorem_ipsum_long" /\
      Group (inj' @"column" top)
        [ String "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas a porttitor ligula. Integer quis neque nec nisi aliquam sodales vitae in tortor. Aliquam erat volutpat. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Sed condimentum felis quis quam pretium, vitae tincidunt est suscipit. Fusce dolor est, rhoncus id sapien ut, fringilla efficitur nibh. Mauris mollis ex sit amet aliquet commodo. Phasellus semper sed neque eu placerat. Morbi et sodales leo, hendrerit pharetra ante. Etiam a cursus lacus. Pellentesque gravida a nisl nec condimentum. Suspendisse scelerisque, nisl vitae posuere posuere, nisl orci egestas leo, non dictum enim est blandit ipsum."
        , String "Fusce velit orci, consequat et enim ut, semper rhoncus sapien. Nam viverra dictum mi sit amet tincidunt. Sed bibendum odio eget erat luctus, nec dictum risus dapibus. Praesent malesuada nibh ac sollicitudin porttitor. Nullam hendrerit nulla venenatis, rhoncus odio eget, suscipit lorem. Etiam sed ex ex. Aenean ac varius ante, id blandit ligula. Sed quis lectus efficitur, tincidunt risus vel, rutrum diam. Nullam in porttitor felis, a porta libero. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae;"
        , String "Sed massa purus, eleifend eu semper ut, egestas vel nunc. In ac aliquam mi, id congue magna. Maecenas sed vulputate purus. Ut tristique nulla eu dolor tincidunt fringilla. Aliquam eget ante fringilla, eleifend justo in, varius nibh. Sed in turpis vel est fringilla ullamcorper. Pellentesque ut dignissim nibh, pellentesque mollis diam. Proin ac vestibulum sem."
        , String "Vivamus dapibus posuere turpis, a suscipit diam. Nunc ut ante tincidunt, cursus nulla quis, consectetur metus. In gravida ipsum vestibulum nibh feugiat, vel tincidunt felis lobortis. Aenean vel sapien ut enim varius pulvinar quis eu mi. Duis sit amet massa sed orci tempus sodales in eget ligula. Aliquam facilisis accumsan pulvinar. Cras in velit ac massa tempor sagittis. Pellentesque sed arcu pulvinar, mollis nunc vitae, porta neque. Mauris ornare dui tempus lorem volutpat, at sodales dolor laoreet. Maecenas volutpat leo velit, id interdum ligula semper ac. Phasellus ut orci diam. Morbi lobortis eros ut justo blandit, mollis pharetra sapien convallis. Mauris consequat, leo eu consectetur finibus, tortor risus mollis nibh, id commodo lacus nisi a tortor. Nam leo lectus, porta porttitor orci sed, mollis mattis nibh."
        , String "Donec id maximus magna. Quisque id posuere tortor, vitae viverra diam. Praesent semper lorem quis vehicula tincidunt. Integer tortor ante, luctus ut risus in, tempor blandit turpis. Ut vitae massa a justo varius pulvinar nec vel lacus. Nulla eu consequat lorem. Praesent elementum consequat elit, vitae aliquet augue placerat porta."
        ]
  , "lorem_ipsum_short" /\
      String "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas a porttitor ligula. Integer quis neque nec nisi aliquam sodales vitae in tortor. Aliquam erat volutpat. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Sed condimentum felis quis quam pretium, vitae tincidunt est suscipit. Fusce dolor est, rhoncus id sapien ut, fringilla efficitur nibh. Mauris mollis ex sit amet aliquet commodo. Phasellus semper sed neque eu placerat. Morbi et sodales leo, hendrerit pharetra ante. Etiam a cursus lacus. Pellentesque gravida a nisl nec condimentum. Suspendisse scelerisque, nisl vitae posuere posuere, nisl orci egestas leo, non dictum enim est blandit ipsum."
  ]

