module Rybl.Language.Component.Doc.Compact where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Reader (class MonadReader, ask, local, runReaderT)
import Control.Monad.State (class MonadState, evalState, get, put)
import Control.Monad.Writer (tell)
import Data.Argonaut (JsonDecodeError)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Lens ((%=), (.=))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (foldMap, traverse)
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Fetch (fetch)
import Fetch as Fetch
import Halogen (Component, defaultEval, mkComponent, mkEval, raise) as H
import Halogen (ComponentHTML, liftAff)
import Halogen.HTML (div) as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JSURI (encodeURI)
import Rybl.Data.Variant (case_, expand, expandCons, inj', inj'U, on')
import Rybl.Halogen.Class as Class
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc(..), Ref)
import Rybl.Language.Component.Common (Ctx, Env, HTML, Input, State, Action, renderDisplayStyle)
import Rybl.Utility (bug, prop', todo, (##), ($@=))
import Type.Prelude (Proxy(..))

renderDoc :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML

renderDoc (Section doc) = do
  title <- renderDoc doc.title
  body <- renderDoc `traverse` doc.body
  pure
    $ HH.div
        [ Style.style $ tell [ "display: flex", "flex-direction: column", "gap: 0.5rem" ] ]
        [ HH.div [] [ title ]
        , HH.div [] body
        ]

renderDoc (Paragraph doc) = do
  body <- renderDoc `traverse` doc.body
  -- TODO: any other special styling for paragraphs?
  pure
    $ HH.div
        [ Style.style $ tell [ "display: flex", "flex-direction: row", "flex-wrap: wrap", "justify-content: flex-start", "align-items: baseline", "align-content: flex-start" ] ]
        body

renderDoc (Sentence doc) = do
  body <- doc.body # traverse renderDoc # map (foldMap (\e -> [ e, HH.text "." ]))
  -- TODO: any other special styling for sentences?
  pure
    $ HH.div
        [ Style.style $ tell [ "display: inline-flex", "flex-direction: row", "flex-wrap: wrap", "justify-content: flex-start", "align-items: baseline", "align-content: flex-start" ] ]
        body

renderDoc (Sidenote doc) = do
  pure
    $ HH.div
        []
        []

renderDoc (Ref doc) = do
  pure
    $ HH.div
        []
        []

renderDoc (String doc) = do
  ctx <- ask
  pure
    $ HH.div
        [ HP.classes [ Class.mk @"string" ], HP.style $ renderDisplayStyle ctx.display ]
        [ HH.text doc ]

renderDoc (Error doc) = do
  ctx <- ask
  -- e <- doc.label # renderDoc
  e <- doc.body # renderDoc
  pure
    $ HH.div
        [ HP.classes [ Class.mk @"error" ], HP.style $ "background-color: #ffcccb; " <> renderDisplayStyle ctx.display ]
        [ e ]

-- renderDoc (Group sty ds) = do
--   ctx <- ask
--   let
--     gap = "0.5em"
--   sty ## case_
--     # on' @"column"
--         ( const do
--             kids <- ds # traverse renderDoc # local (_ { display = inj'U @"block" })
--             pure $
--               HH.div
--                 [ HP.classes [ Class.mk @"group", Class.mk @"group_column" ], Style.style $ tell [ "display: flex", "flex-direction: column", "gap: " <> gap ] ]
--                 kids

--         )
--     # on' @"row"
--         ( const do
--             kids <- ds # traverse renderDoc # local (_ { display = inj'U @"block" })
--             pure
--               $ HH.div
--                   [ HP.classes [ Class.mk @"group", Class.mk @"group_row" ], Style.style $ tell [ "display: flex", "flex-direction: row", "gap: " <> gap ] ]
--                   kids

--         )
--     # on' @"flow"
--         ( const do
--             kids <- ds # traverse renderDoc # local (_ { display = inj'U @"inline" }) # map (Array.intersperse $ HH.text " ")
--             pure $
--               HH.div
--                 [ HP.classes [ Class.mk @"group", Class.mk @"group_flow" ], HP.style $ renderDisplayStyle ctx.display ]
--                 kids
--         )

-- renderDoc (Ref x) = do
--   { namedDocs } <- ask
--   case namedDocs # Map.lookup x of
--     Nothing -> pure
--       $ HH.div
--           [ HP.classes [ Class.mk @"ref", Class.mk @"ref_missing" ] ]
--           [ HH.text $ "missing ref: " <> show x ]
--     Just a -> a ## case_
--       # on' @"loaded" renderDoc
--       # on' @"not_yet_loaded"
--           ( const $ pure
--               $ HH.div
--                   [ HP.classes [ Class.mk @"ref", Class.mk @"ref_loading" ] ]
--                   [ HH.text $ "loading ref " <> show x ]

--           )
--       # on' @"error_on_load"
--           ( \err -> pure
--               $ HH.div
--                   [ HP.classes [ Class.mk @"ref", Class.mk @"ref_error" ]
--                   , HP.style "display: flex; flex-direction: column; background-color: #ffcccb;"
--                   ]
--                   [ HH.text $ "error when loading ref " <> show x
--                   , err # HH.fromPlainHTML
--                   ]
--           )

renderDoc (Link link_) = todo ""

-- renderDoc (Link link_) = link_ ## case_
--   # on' @"external"
--       ( \link ->
--           pure $
--             HH.a
--               [ HP.classes [ Class.mk @"link", Class.mk @"link_external" ]
--               , Style.style $ tell
--                   [ "display: inline-flex"
--                   , "flex-direction: row"
--                   , "align-items: baseline"
--                   , "gap: 0.2em"
--                   ]
--               , HP.href link.href
--               ]
--               case link.mb_favicon_src of
--                 Nothing ->
--                   [ HH.div_ [ HH.text link.label ] ]
--                 Just favicon_src ->
--                   [ HH.img
--                       [ Style.style $ tell [ "height: 0.8em" ]
--                       , HP.src favicon_src
--                       ]
--                   , HH.div_ [ HH.text link.label ]
--                   ]
--       )
--   # on' @"ref"
--       ( \link -> do
--           pure $
--             HH.a
--               [ HP.classes [ Class.mk @"link", Class.mk @"link_external" ]
--               , Style.style $ tell
--                   [ "display: inline-flex"
--                   , "flex-direction: row"
--                   , "align-items: baseline"
--                   , "gap: 0.2em"
--                   ]
--               , HP.href $ "/index.html?doc=" <> (Ref link.ref # toJsonString # encodeURI # fromMaybe' \_ -> bug $ "failed: encodeURI " <> show (Ref link.ref # toJsonString))
--               ]
--               [ HH.img
--                   [ Style.style $ tell [ "height: 0.8em" ]
--                   , HP.src "/favicon.ico"
--                   ]
--               , HH.div_ [ HH.text link.label ]
--               ]
--       )

-- renderDoc (Expander sty label_ body_) = do
--   widgetIndex <- nextSlotIndex
--   label <- renderDoc label_
--   body <- renderDoc body_
--   pure $ HH.slot (Proxy @"widget") widgetIndex theExpanderComponent { sty, label, body } expand

-- renderDoc (Sidenote id_ label_ _body) = do
--   ctx <- ask
--   id <- local (_ { display = inj'U @"inline" }) $ renderSidenoteId id_
--   label <- renderDoc label_
--   pure $
--     HH.div
--       [ HP.classes [ Class.mk @"sidenote_label" ]
--       , Style.style $ tell [ renderDisplayStyle ctx.display ]
--       ]
--       [ HH.div
--           [ Style.style $ tell [ "display: inline" ] ]
--           [ label ]
--       , HH.text " "
--       , HH.div
--           [ Style.style $ tell [ "display: inline", "cursor: pointer" ]
--           , HE.onClick (const (inj' @"modify_ctx" _ { mb_target_sidenote_id = pure id_ }))
--           ]
--           [ id ]
--       ]

-- renderDoc (SidenotesThreshold body_) = do
--   ctx <- ask
--   body <- renderDoc body_
--   sidenotes <- collectSidenotes body_ $@= \sidenote -> do
--     sidenote_id <- renderSidenoteId sidenote.id
--     sidenote_body <- renderDoc sidenote.body
--     pure $
--       HH.div
--         [ HP.classes [ Class.mk @"sidenote" ]
--         , HP.id sidenote.id
--         , Style.style $ tell [ "display: flex", "flex-direction: row", "gap: 0.2em" ]
--         ]
--         [ sidenote_id
--         , HH.div
--             [ HP.classes [ Class.mk @"sidenote_body" ]
--             , Style.style do
--                 -- TODO: does this actually work not as a class toggle?
--                 tell [ "transition-property: box-shadow", "transition-timing-function: linear", "transition-duration: 0.2s" ]
--                 when (ctx.mb_target_sidenote_id == pure sidenote.id) do tell [ "box-shadow: 0 0 0 0.2em red" ]
--             ]
--             [ sidenote_body ]
--         ]
--   pure $
--     HH.div
--       [ HP.classes [ Class.mk @"sidenote_threshold" ]
--       , Style.style $ tell [ "display: flex", "flex-direction: row", "justify-content: space-between" ]
--       ]
--       [ HH.div
--           [ HP.classes [ Class.mk @"sidenote_threshold_body" ]
--           , Style.style $ tell [ "width: 500px" ]
--           ]
--           [ body ]
--       , HH.div
--           [ HP.classes [ Class.mk @"sidenote_threshold_sidenotes" ]
--           , Style.style $ tell [ "width: 250px", "display: flex", "flex-direction: column", "gap: 1em" ]
--           ]
--           sidenotes
--       ]

renderSidenoteId :: forall m. MonadReader Ctx m => MonadState Env m => String -> m HTML
renderSidenoteId id = do
  ctx <- ask
  pure $
    HH.div
      [ HP.classes [ Class.mk @"sidenote_id" ]
      , Style.style $ tell [ renderDisplayStyle ctx.display, "margin: 0 0.2em", "padding: 0 0.2em", "color: red", "background-color: lightgray" ]
      ]
      [ HH.text id ]

--------------------------------------------------------------------------------
-- theExpanderComponent
--------------------------------------------------------------------------------

-- theExpanderComponent :: forall query. H.Component query { sty :: ExpanderStyle, label :: HTML, body :: HTML } Action Aff
-- theExpanderComponent = H.mkComponent { initialState, eval, render }
--   where
--   initialState { sty, label, body } =
--     { sty, label, body, is_open: false }

--   eval = H.mkEval H.defaultEval
--     { receive = pure <<< inj' @"receive"
--     , handleAction = case_
--         # on' @"receive" (put <<< initialState)
--         # on' @"toggle_is_open" (const (prop' @"is_open" %= not))
--         # on' @"modify_env" (\f -> H.raise (inj' @"modify_env" f))
--         # on' @"modify_ctx" (\f -> H.raise (inj' @"modify_ctx" f))
--     }

--   render { sty: _, label, body, is_open } =
--     HH.div
--       [ HP.classes [ Class.mk @"expander" ]
--       , Style.style $ tell [ "box-shadow: 0 0 0 0.1em black inset", "display: flex", "flex-direction: column", "padding: 0.5em", "gap: 0.5em" ]
--       ]
--       [ label # mapAction_ComponentHTML (expandCons @"toggle_is_open")
--       , HH.button
--           [ HP.classes [ Class.mk @"expander_button" ], HE.onClick $ const $ inj'U @"toggle_is_open" ]
--           [ HH.text "expand" ]
--       , if is_open then
--           body # mapAction_ComponentHTML (expandCons @"toggle_is_open")
--         else
--           HH.div
--             [ HP.classes [ Class.mk @"expander_placeholder_content" ] ]
--             [ HH.text "..." ]
--       ]
--       # mapAction_ComponentHTML (expandCons @"receive")

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

mapAction_ComponentHTML
  :: forall action action' slots m
   . (action -> action')
  -> ComponentHTML action slots m
  -> ComponentHTML action' slots m
mapAction_ComponentHTML f = bimap (map f) f

