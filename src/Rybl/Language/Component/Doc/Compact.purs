module Rybl.Language.Component.Doc.Compact where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, get)
import Control.Monad.Writer (tell)
import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, intercalate, length)
import Data.Int as Int
import Data.Lens ((%=), (%~), (.=))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Traversable (foldMap, traverse)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JSURI (encodeURI)
import Rybl.Data.Fix (Fix(..))
import Rybl.Data.Fix as Fix
import Rybl.Data.Tree (Tree(..))
import Rybl.Data.Variant (match)
import Rybl.Halogen.Style as Style
import Rybl.Language (Doc, Doc_(..), Resource)
import Rybl.Language as RL
import Rybl.Language.Component.Common (Ctx, Env, HTML, next_widget_index, mapAction_ComponentHTML)
import Rybl.Utility (bug, prop', todo)
import Type.Proxy (Proxy(..))

renderDoc :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML

renderDoc doc@(Fix (Page opts args body_)) = do
  title <- renderDoc $ RL.string {} args.title
  body <- body_ # traverse renderDoc
  tableOfContents <- doc # renderTableOfContents
  bibliography <- doc # renderBibliography
  let id = opts.id # fromMaybe' \_ -> bug $ "page was not given an id: " <> show args.title
  pure $
    HH.div
      [ Style.style $ tell [ "display: flex", "flex-direction: column", "gap: 1.0em" ]
      , HP.id id
      ]
      [ HH.div
          [ Style.style $ tell
              [ "display: flex"
              , "flex-direction: row"
              , "justify-content: space-between"
              , "align-items: flex-start"
              , "gap: 1em"
              , "font-size: 3em"
              ]
          ]
          [ HH.div_
              [ HH.a
                  [ HP.href $ "#" <> id ]
                  [ HH.text $ "§" ]
              ]
          , HH.div_
              [ title ]
          , HH.div_
              [ HH.a
                  [ HP.href $ "#" <> id ]
                  [ HH.text $ "§" ]
              ]
          ]
      , HH.div
          [ Style.style do tell [ "border: 1px solid black", "padding: 1em" ] ]
          [ tableOfContents ]
      , HH.div
          [ Style.style $ tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
          body
      , HH.div
          [ Style.style do tell [ "border: 1px solid black", "padding: 1em" ] ]
          [ bibliography ]
      ]

renderDoc (Fix (Section opts args body_)) = do
  { section_path } <- ask
  { section_index } <- get
  let section_depth = section_path # length
  title <- RL.string {} args.title # renderDoc
  body <-
    local (prop' @"section_path" %~ List.Cons { index: section_index, title: args.title }) do
      prop' @"section_index" .= 0
      body_ # traverse renderDoc -- # map Array.fold
  let id = opts.id # fromMaybe' \_ -> bug $ "section was not given an id: " <> show args.title
  pure $
    HH.div
      [ Style.style $ tell [ "padding-top: 1em;", "display: flex", "flex-direction: column", "gap: 0.5em" ]
      , HP.id id
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
              [ Style.style $ tell [ "flex-grow: 0" ] ] $
              [ HH.a
                  [ HP.href $ "#" <> id ]
                  [ HH.text $ "§" ]
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
                  [ HP.href $ "#" <> id ]
                  [ HH.text $ "§" ]
              ]
          ]
      , HH.div
          [ Style.style $ tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
          body
      ]

renderDoc (Fix (Paragraph _opts _args body_)) = do
  bodys <- body_ # traverse renderDoc -- TODO: -- (foldMap \x -> [ x, [ HH.text " " ] ]) # map Array.fold
  let body = bodys # Array.intersperse (HH.text " ")
  pure $ HH.div_
    body

renderDoc (Fix (Sentence _opts _args body_)) = do
  body <- body_ # traverse renderDoc
  pure $
    HH.div
      [ Style.style $ tell [ "display: inline" ] ]
      body

renderDoc (Fix (Sidenote _opts _args label_ body_)) = do
  label <- label_ # renderDoc
  body <- body_ # renderDoc
  widget_index <- next_widget_index
  pure $
    HH.slot_ (Proxy @"SidenoteExpander") widget_index theSidenoteExpanderComponent
      { label: label # mapAction_ComponentHTML Left
      , body: body # mapAction_ComponentHTML Left
      }

renderDoc (Fix (Ref _opts args)) = do
  pure $
    HH.div
      [ Style.style do tell [ "background-color: black", "color: white", "padding: 0.5em" ] ]
      [ HH.text $ "Ref " <> show args.refId ]

renderDoc (Fix (CodeBlock _opts args)) = do
  pure $
    HH.div
      [ Style.style do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
      [ HH.pre
          [ Style.style do tell [ "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)", "overflow-x: scroll" ] ]
          [ HH.text args.value ]
      ]

renderDoc (Fix (QuoteBlock _opts _args body_)) = do
  body <- body_ # renderDoc
  pure $
    HH.div
      [ Style.style do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
      [ HH.div
          [ Style.style do
              tell
                [ "margin: 0 1em"
                , "padding: 0.5em"
                , "border-left: 4px solid black"
                , "background-color: color-mix(in hsl, teal, transparent 80%)"
                , "border-radius: 1em"
                ]
          ]
          [ body ]
      ]

renderDoc (Fix (MathBlock _opts args)) = do
  pure $
    HH.div
      [ Style.style do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
      [ HH.pre
          [ Style.style do tell [ "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)", "overflow-x: scroll" ] ]
          [ HH.text $ "MATH: " <> args.value ]
      ]

renderDoc (Fix (Image opts args caption__)) = do
  caption_ <- caption__ # traverse renderDoc
  resource_ <- opts.source # traverse renderResource
  pure
    $ HH.div
        [ Style.style do tell [ "width: 100%", "display: flex", "flex-direction: column", "justify-content: center" ] ]
    $ fold
        [ [ HH.img
              [ Style.style do tell [ "width: 100%" ]
              , HP.src args.url
              ]
          ]
        , caption_ # maybe [] \caption ->
            [ HH.div
                [ Style.style $ do tell [ "margin: 0 0.5em", "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)" ] ]
                [ caption ]
            ]
        , resource_ # maybe [] \resource ->
            [ HH.div
                [ Style.style $ do tell [ "margin: 0 1em", "padding: 0.5em", "background-color: color-mix(in hsl, saddlebrown, transparent 80%)" ] ]
                [ resource ]
            ]
        ]

renderDoc (Fix (String opts args)) = do
  pure $
    HH.div
      [ Style.style do
          tell [ "display: inline" ]
          opts.style # maybe (pure unit)
            ( match
                { emphasis: const do
                    tell [ "font-weight: bold" ]
                , code: const do
                    tell [ "font-family: monospace" ]
                }
            )
      ]
      [ HH.text args.value ]

renderDoc (Fix (Error _opts _args body_)) = do
  e <- body_ # renderDoc
  pure $
    HH.div
      [ Style.style $ tell [ "background-color: #ffcccb" ] ]
      [ e ]

renderDoc (Fix (ExternalLink opts args label_)) = do
  label <- label_ # renderDoc
  pure $
    HH.a
      [ Style.style $ tell
          [ "display: inline-flex"
          , "flex-direction: row"
          , "align-items: baseline"
          , "gap: 0.2em"
          ]
      , HP.href args.url
      ]
      case opts.favicon_url of
        Nothing ->
          [ HH.div_ [ label ] ]
        Just favicon_src ->
          [ HH.img
              [ Style.style $ tell [ "height: 0.8em" ]
              , HP.src favicon_src
              ]
          , HH.div_ [ label ]
          ]

renderDoc (Fix (InternalLink _opts args label_)) = do
  label <- label_ # renderDoc
  pure $
    HH.a
      [ Style.style $ tell
          [ "display: inline-flex"
          , "flex-direction: row"
          , "align-items: baseline"
          , "gap: 0.2em"
          ]
      , HP.href $ "/index.html?ref=" <> (args.refId # unwrap # encodeURI # fromMaybe' \_ -> bug $ "failed: encodeURI " <> show (args.refId # toJsonString))
      ]
      [ HH.img
          [ Style.style $ tell [ "height: 0.8em" ]
          , HP.src "/favicon.ico"
          ]
      , HH.div_ [ label ]
      ]

renderResource :: forall m. MonadReader Ctx m => MonadState Env m => Resource -> m HTML
renderResource res = do
  pure
    $ HH.div_
    $ intercalate [ HH.text " • " ]
        [ res.name # maybe [] \name -> [ HH.text $ name ]
        , res.date # maybe [] \date -> [ HH.i_ [ HH.text "accessed " ], HH.text $ date ]
        , res.source # maybe []
            ( match
                { url: \url -> [ HH.a [ HP.href url ] [ HH.text url ] ]
                , misc: \str -> [ HH.text str ]
                }
            )
        ]

renderTableOfContents :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML
renderTableOfContents doc = do
  let
    tocTree :: Tree _
    tocTree = doc # Fix.fold case _ of
      Page opts args body -> Branch { id: opts.id, title: args.title } body
      Section opts args body -> Branch { id: opts.id, title: args.title } body
      _ -> Leaf

    go :: Tree _ -> Array HTML
    go Leaf = []
    go (Branch node kids) =
      let
        kids' = kids # map go
      in
        [ HH.div
            [ Style.style do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
            [ HH.div_
                [ HH.a
                    ([ node.id # maybe [] \id -> [ HP.href $ "#" <> id ] ] # fold)
                    [ HH.text node.title ]
                ]
            , HH.div
                [ Style.style do tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "padding-left: 1em" ] ]
                (kids' # fold)
            ]
        ]

  pure $
    HH.div_
      (go tocTree)

renderBibliography :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML
renderBibliography doc = do
  pure $
    HH.div_
      [ HH.text "{{bibliography}}" ]

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
      bgcolor = "rgba(0, 0, 0, 0.1)"
    in
      HH.div
        [ Style.style $ tell [ "display: inline" ] ] $ fold $
        [ [ HH.div
              [ Style.style $ tell [ "display: inline", "user-select: none", "cursor: pointer", "background-color: " <> bgcolor, "padding-left: 0.3em" ]
              , HE.onClick $ const $ Right unit
              ] $ fold $
              [ marker
              , [ HH.text " " ]
              , [ state.label ]
              ]
          ]
        , if not state.open then []
          else
            [ HH.div
                [ Style.style $ tell
                    [ "margin: 1.0em"
                    , "padding: 0.5em"
                    -- , "box-shadow: 0 0 0.5em 0 rgba(0, 0, 0, 0.5)" -- shadow
                    , "border: 2px dashed black"
                    , "background-color: " <> bgcolor
                    ]
                ]
                [ state.body ]
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

