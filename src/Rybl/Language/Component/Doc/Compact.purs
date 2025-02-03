module Rybl.Language.Component.Doc.Compact where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, get)
import Control.Monad.Writer (tell)
import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, length)
import Data.Int as Int
import Data.Lens ((%=), (%~), (.=))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (foldMap, traverse)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JSURI (encodeURI, encodeURIComponent)
import Rybl.Data.Variant (case_, match, on')
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc(..))
import Rybl.Language as RL
import Rybl.Language.Component.Common (Ctx, Env, HTML, next_widget_index, mapAction_ComponentHTML)
import Rybl.Utility (bug, impossible, prop', (##))
import Type.Proxy (Proxy(..))

renderDoc :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m (Array HTML)

renderDoc (Section doc) = do
  { section_path } <- ask
  { section_index } <- get
  let section_depth = section_path # length
  title <- renderDoc $ RL.string doc.title
  body <-
    local (prop' @"section_path" %~ List.Cons { index: section_index, title: doc.title }) do
      prop' @"section_index" .= 0
      doc.body # traverse renderDoc # map Array.fold
  prop' @"section_index" .= section_index + 1
  let
    section_id =
      "section_" <>
        ( doc.title
            # String.replace (String.Pattern " ") (String.Replacement "_")
            # encodeURIComponent >>> fromMaybe' impossible
        )
  pure
    [ HH.div
        [ Style.style $ tell [ "padding-top: 1rem;", "display: flex", "flex-direction: column", "gap: 0.5rem" ]
        , HP.id section_id
        ]
        [ HH.div
            [ Style.style $ tell
                [ "display: flex"
                , "flex-direction: row"
                , "justify-content: space-between"
                , "align-items: flex-start"
                , "gap: 1em"
                , "font-size: " <> show ((2.0 - (Int.toNumber section_depth * 0.2)) `max` 1.0) <> "em"
                , "box-shadow: 0 1px 0 0 black"
                ]
            ]
            [ HH.div
                [ Style.style $ tell [ "flex-grow: 0" ] ] $ fold $
                [ [ HH.a
                      [ HP.href $ "#" <> section_id ]
                      [ HH.text $ "§" ]
                  ]
                , title
                ]
            , HH.div
                [ Style.style $ tell [ "flex-shrink: 0", "flex-grow: 1", "text-align: right" ] ]
                [ HH.text
                    $ List.Cons section_index (section_path # map _.index)
                        # map ((_ + 1) >>> show)
                        # List.reverse
                        # List.intercalate "."
                , HH.a
                    [ HP.href $ "#" <> section_id ]
                    [ HH.text $ "§" ]
                ]
            ]
        , HH.div
            [ Style.style $ tell [ "display: flex", "flex-direction: column", "gap: 0.5rem" ] ]
            body
        ]
    ]

renderDoc (Paragraph doc) = do
  body <- doc.body # traverse renderDoc # map (foldMap \x -> [ x, [ HH.text " " ] ]) # map Array.fold
  pure
    [ HH.div
        [ Style.style $ tell [] ]
        body
    ]

renderDoc (Sentence doc) = do
  body <- doc.body # traverse renderDoc # map Array.fold
  pure
    [ HH.div
        [ Style.style $ tell [ "display: inline" ] ]
        body
    ]

renderDoc (Sidenote doc) = do
  label <- doc.label # renderDoc
  body <- doc.body # renderDoc
  widget_index <- next_widget_index
  pure
    [ HH.slot_ (Proxy @"SidenoteExpander") widget_index theSidenoteExpanderComponent
        { label: label # map (mapAction_ComponentHTML Left)
        , body: body # map (mapAction_ComponentHTML Left)
        }
    ]

renderDoc (Ref doc) = do
  { namedDocs } <- ask
  case namedDocs # Map.lookup doc.refId of
    Nothing -> pure
      [ HH.div
          []
          [ HH.text $ "missing refId: " <> show doc.refId ]
      ]
    Just a -> a ## case_
      # on' @"loaded" renderDoc
      # on' @"not_yet_loaded"
          ( const $ pure
              [ HH.div
                  []
                  [ HH.text $ "loading refId " <> show doc.refId ]
              ]
          )
      # on' @"error_on_load"
          ( \err -> pure
              [ HH.div
                  [ Style.style $ tell [ "display: flex", "flex-direction: column", "background-color: #ffcccb" ] ]
                  [ HH.text $ "error when loading refId " <> show doc.refId
                  , err # HH.fromPlainHTML
                  ]
              ]
          )

renderDoc (String doc) = do
  pure
    [ HH.div
        [ Style.style do
            tell [ "display: inline" ]
            doc.style # match
              { plain: mempty
              , emphasis: const do
                  tell [ "font-weight: bold" ]
              , code: const do
                  tell [ "font-family: monospace" ]
              }
        ]
        [ HH.text doc.value ]
    ]

renderDoc (Error doc) = do
  e <- doc.body # renderDoc
  pure
    [ HH.div
        [ Style.style $ tell [ "background-color: #ffcccb" ] ]
        e
    ]

renderDoc (Link doc) = doc.src ## case_
  # on' @"external"
      ( \src -> do
          label <- doc.label # renderDoc
          pure
            [ HH.a
                [ Style.style $ tell
                    [ "display: inline-flex"
                    , "flex-direction: row"
                    , "align-items: baseline"
                    , "gap: 0.2em"
                    ]
                , HP.href src.href
                ]
                case src.mb_favicon_src of
                  Nothing ->
                    [ HH.div_ label ]
                  Just favicon_src ->
                    [ HH.img
                        [ Style.style $ tell [ "height: 0.8em" ]
                        , HP.src favicon_src
                        ]
                    , HH.div_ label
                    ]
            ]
      )
  # on' @"internal"
      ( \src -> do
          label <- doc.label # renderDoc
          pure
            [ HH.a
                [ Style.style $ tell
                    [ "display: inline-flex"
                    , "flex-direction: row"
                    , "align-items: baseline"
                    , "gap: 0.2em"
                    ]
                , HP.href $ "/index.html?ref=" <> (src.refId # unwrap # encodeURI # fromMaybe' \_ -> bug $ "failed: encodeURI " <> show (src.refId # toJsonString))
                ]
                [ HH.img
                    [ Style.style $ tell [ "height: 0.8em" ]
                    , HP.src "/favicon.ico"
                    ]
                , HH.div_ label
                ]
            ]
      )

--------------------------------------------------------------------------------
-- theSidenoteExpanderComponent
--------------------------------------------------------------------------------

theSidenoteExpanderComponent :: H.Component _ _ _ Aff
theSidenoteExpanderComponent = H.mkComponent { initialState, eval, render }
  where
  initialState input =
    { label: input.label
    , body: input.body
    , open: false
    }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction (Left action) = H.raise action
  handleAction (Right _) = do
    Console.log "toggling expander"
    prop' @"open" %= not

  render state =
    let
      marker = if state.open then [ HH.text "■" ] else [ HH.text "□" ]
    in
      HH.div
        [ Style.style $ tell [ "display: inline" ] ] $ fold $
        [ [ HH.div
              [ Style.style $ tell [ "display: inline", "user-select: none", "cursor: pointer", "background-color: rgba(0, 0, 0, 0.1)", "padding-left: 0.3em" ]
              , HE.onClick $ const $ Right unit
              ] $ fold $
              [ marker
              , [ HH.text " " ]
              , state.label
              ]
          ]
        , if not state.open then []
          else
            [ HH.div
                [ Style.style $ tell [ "margin: 0.5rem", "padding: 0.5rem", "box-shadow: 0 0 0.5em 0 rgba(0, 0, 0, 0.5)", "background-color: rgba(0, 0, 0, 0.1)" ] ]
                state.body
            ]
        , [ HH.div
              [ Style.style $ tell [ "display: inline", "user-select: none", "cursor: pointer", "background-color: rgba(0, 0, 0, 0.1)", "padding-right: 0.3em" ]
              , HE.onClick $ const $ Right unit
              ] $ fold $
              [ [ HH.text " " ]
              , marker
              ]
          ]
        ]

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
--       [ HP.classes [  ]
--       , Style.style $ tell [ "box-shadow: 0 0 0 0.1em black inset", "display: flex", "flex-direction: column", "padding: 0.5em", "gap: 0.5em" ]
--       ]
--       [ label # mapAction_ComponentHTML (expandCons @"toggle_is_open")
--       , HH.button
--           [ HP.classes [  ], HE.onClick $ const $ inj'U @"toggle_is_open" ]
--           [ HH.text "expand" ]
--       , if is_open then
--           body # mapAction_ComponentHTML (expandCons @"toggle_is_open")
--         else
--           HH.div
--             [ HP.classes [  ] ]
--             [ HH.text "..." ]
--       ]
--       # mapAction_ComponentHTML (expandCons @"receive")

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- mapAction_ComponentHTML
--   :: forall action action' slots m
--    . (action -> action')
--   -> ComponentHTML action slots m
--   -> ComponentHTML action' slots m
-- mapAction_ComponentHTML f = bimap (map f) f

