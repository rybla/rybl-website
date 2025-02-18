module Rybl.Language.Component.Doc.Compact where

import Prelude

import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.State (class MonadState, get)
import Control.Monad.Writer (tell)
import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, length, null)
import Data.Int as Int
import Data.Lens ((%=), (%~), (.=))
import Data.List as List
import Data.Maybe (fromMaybe', maybe, maybe')
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Unfoldable (none)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JSURI (encodeURI)
import Rybl.Data.Fix (Fix(..))
import Rybl.Data.Fix as Fix
import Rybl.Data.Tree (Tree(..), isLeaf)
import Rybl.Data.Variant (match)
import Rybl.Halogen.Style as Style
import Rybl.Language (Citation(..), Doc, Doc_(..), Resource(..))
import Rybl.Language as RL
import Rybl.Language.Component.Common (Ctx, Env, HTML, mapAction_ComponentHTML, next_widget_index)
import Rybl.Utility (bug, prop', todo)
import Type.Proxy (Proxy(..))

renderDoc :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML

renderDoc doc@(Fix (Page _opts prms body_)) = do
  body <- body_ # traverse renderDoc
  tableOfContents <- doc # renderTableOfContents
  bibliography <- doc # renderBibliography
  pure $
    HH.div
      [ Style.css $ tell [ "display: flex", "flex-direction: column", "gap: 1.0em" ]
      , HP.id prms.id
      ]
      [ HH.div
          [ Style.css do tell [ "font-size: 3em", "text-align: center" ] ]
          [ HH.text prms.title ]
      , HH.div
          [ Style.css $ tell [ "display: flex", "flex-direction: column", "gap: 2em" ] ] $ fold $
          [ [ tableOfContents ]
          , body
          , [ bibliography ]
          ]
      ]

renderDoc (Fix (Section _opts prms body_)) = do
  { section_path } <- ask
  { section_index } <- get
  let section_depth = section_path # length
  -- title <- RL.string {} prms.title # renderDoc
  title <- renderSectionTitle
    { title: HH.text prms.title
    , id: prms.id
    , section_depth
    , section_index:
        List.Cons section_index (section_path # map _.index)
          # map ((_ + 1) >>> show)
          # List.reverse
          # List.intercalate "."
          # pure
    }
  body <-
    local (prop' @"section_path" %~ List.Cons { index: section_index, title: prms.title }) do
      prop' @"section_index" .= 0
      body_ # traverse renderDoc -- # map Array.fold
  prop' @"section_index" .= section_index + 1
  pure $
    HH.div
      [ Style.css $ tell [ "display: flex", "flex-direction: column", "gap: 1.5em" ]
      , HP.id prms.id
      ]
      [ title
      -- HH.div
      --   [ Style.css $ tell
      --       [ "display: flex"
      --       , "flex-direction: row"
      --       , "justify-content: space-between"
      --       , "align-items: flex-start"
      --       , "gap: 1em"
      --       , "font-size: " <> show ((2.0 - (Int.toNumber section_depth * 0.2)) `max` 1.0) <> "em"
      --       , "box-shadow: 0 1px 0 0 black"
      --       ]
      --   ]
      --   [ HH.div
      --       [ Style.css $ tell [ "flex-grow: 0" ] ] $
      --       [ HH.a
      --           [ HP.href $ "#" <> id ]
      --           [ HH.text $ "§" ]
      --       , title
      --       ]
      --   , HH.div
      --       [ Style.css $ tell [ "flex-shrink: 0", "flex-grow: 1", "text-align: right" ] ]
      --       [ HH.text
      --           $ List.Cons section_index (section_path # map _.index)
      --               # map ((_ + 1) >>> show)
      --               # List.reverse
      --               # List.intercalate "."
      --       , HH.a
      --           [ HP.href $ "#" <> id ]
      --           [ HH.text $ "§" ]
      --       ]
      --   ]
      , HH.div
          [ Style.css $ tell [ "display: flex", "flex-direction: column", "gap: 1em" ] ]
          body
      ]

renderDoc (Fix (Paragraph _opts _prms body_)) = do
  bodys <- body_ # traverse renderDoc -- TODO: -- (foldMap \x -> [ x, [ HH.text " " ] ]) # map Array.fold
  let body = bodys # Array.intersperse (HH.text " ")
  pure $ HH.div_
    body

renderDoc (Fix (Sentence _opts _prms body_)) = do
  body <- body_ # traverse renderDoc
  pure $
    HH.div
      [ Style.css $ tell [ "display: inline" ] ]
      body

renderDoc (Fix (Sidenote _opts _prms label_ body_)) = do
  label <- label_ # renderDoc
  body <- body_ # renderDoc
  widget_index <- next_widget_index
  pure $
    HH.slot_ (Proxy @"SidenoteExpander") widget_index theSidenoteExpanderComponent
      { label: label # mapAction_ComponentHTML Left
      , body: body # mapAction_ComponentHTML Left
      }

renderDoc (Fix (Ref _opts prms)) = do
  pure $
    HH.div
      [ Style.css do tell [ "background-color: black", "color: white", "padding: 0.5em" ] ]
      [ HH.text $ "Ref " <> show prms.refId ]

renderDoc (Fix (CodeBlock opts prms)) = do
  source <- opts.citation # traverse renderCitation
  pure
    $ HH.div
        []
    $ fold
        [ [ HH.div
              [ Style.css do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
              [ HH.pre
                  [ Style.css do tell [ "margin: 0", "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)", "overflow-x: scroll" ] ]
                  [ HH.text prms.value ]
              ]
          ]
        , source # maybe [] pure
        ]

renderDoc (Fix (QuoteBlock opts _prms body_)) = do
  body <- body_ # renderDoc
  source <- opts.citation # traverse renderCitation
  pure
    $ HH.div
        [ Style.css do tell [ "display: flex", "flex-direction: column" ] ]
    $ fold
        [ [ HH.div
              [ Style.css do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
              [ HH.div
                  [ Style.css do
                      tell
                        [ "margin: 0 1em"
                        , "padding: 0.5em"
                        , "border-left: 4px solid black"
                        , "background-color: color-mix(in hsl, teal, transparent 90%)"
                        , "border-radius: 1em"
                        ]
                  ]
                  [ body ]
              ]
          ]
        , source # maybe [] pure
        ]

renderDoc (Fix (MathBlock opts prms)) = do
  source <- opts.citation # traverse renderCitation
  pure
    $ HH.div
        []
    $ fold
        [ [ HH.div
              [ Style.css do tell [ "display: flex", "flex-direction: row", "justify-content: center" ] ]
              [ HH.pre
                  [ Style.css do tell [ "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)", "overflow-x: scroll" ] ]
                  [ HH.text $ "MATH: " <> prms.value ]
              ]
          ]
        , source # maybe [] pure
        ]

renderDoc (Fix (Image opts prms caption__)) = do
  caption_ <- caption__ # traverse renderDoc
  source <- opts.citation # traverse renderCitation
  pure
    $ HH.div
        [ Style.css do tell [ "width: 100%", "display: flex", "flex-direction: column", "justify-content: center" ] ]
    $ fold
        [ [ HH.img
              [ Style.css do tell [ "width: 100%" ]
              , HP.src prms.url
              ]
          ]
        , caption_ # maybe [] \caption ->
            [ HH.div
                [ Style.css $ do tell [ "margin: 0 0.5em", "padding: 0.5em", "background-color: rgba(0, 0, 0, 0.1)" ] ]
                [ caption ]
            ]
        , source # maybe [] pure
        ]

renderDoc (Fix (String opts prms)) = do
  pure $
    HH.div
      [ Style.css do
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
      [ HH.text prms.value ]

renderDoc (Fix (Error _opts _prms body_)) = do
  e <- body_ # renderDoc
  pure $
    HH.div
      [ Style.css $ tell [ "background-color: #ffcccb" ] ]
      [ e ]

renderDoc (Fix (LinkExternal opts prms label_)) = do
  label <- label_ # renderDoc
  let
    favicon = opts.favicon_url # maybe [] \favicon_url ->
      [ HH.img
          [ Style.css $ tell [ "height: 0.8em" ]
          , HP.src favicon_url
          ]
      ]
    notes = opts.url # flip maybe' (const []) \_ -> [ HH.div [] [ nub ] ]
  pure
    $ HH.a
        ( [ [ Style.css $ tell
                [ "display: inline-flex"
                , "flex-direction: row"
                , "align-items: baseline"
                , "gap: 0.2em"
                , "box-shadow: 0 1px 0 0 blue"
                ]
            ]
          , opts.url # maybe [] (HP.href >>> pure)
          ] # fold
        )
    $ fold
        [ favicon
        , [ HH.div_ [ label ] ]
        , notes
        ]

renderDoc (Fix (LinkInternal opts _prms label_)) = do
  label <- label_ # renderDoc
  let
    favicon =
      [ HH.img
          [ Style.css $ tell [ "height: 0.8em" ]
          , HP.src "/favicon.ico"
          ]
      ]
    notes = opts.refId # flip maybe' (const []) \_ -> [ HH.div [] [ nub ] ]
  pure
    $ HH.a
        ( [ [ Style.css $ tell
                [ "display: inline-flex"
                , "flex-direction: row"
                , "align-items: baseline"
                , "gap: 0.2em"
                , "box-shadow: 0 1px 0 0 blue"
                ]
            ]
          , opts.refId # maybe [] \refId -> [ HP.href $ "/index.html?ref=" <> (refId # unwrap # encodeURI # fromMaybe' \_ -> bug $ "failed: encodeURI " <> show (refId # toJsonString)) ]
          ] # fold
        )
    $ fold
        [ favicon
        , [ HH.div_ [ label ] ]
        , notes
        ]

nub :: HTML
nub =
  HH.sup [ Style.css do tell [ "display: inline-block", "color: white", "background-color: red", "padding: 0 0.2em" ] ]
    [ HH.text "nub" ]

renderResource :: forall m. MonadReader Ctx m => MonadState Env m => Resource -> m HTML
renderResource (Resource opts prms) = do
  pure
    $ HH.div [ Style.css do tell [ "word-wrap: word-break" ] ]
    $ Array.intersperse (HH.text " • ")
    $ fold
        [ [ HH.text $ prms.name ]
        -- TODO: use something like this in renderCitation
        -- , opts.date # maybe [] \date -> [ HH.span_ [ HH.i_ [ HH.text "accessed " ], HH.text $ date ] ]
        , opts.content # maybe []
            ( match
                { url: \url ->
                    [ HH.a
                        [ HP.href url
                        , Style.css do tell [ "word-wrap: word-break" ]
                        ]
                        [ HH.text "url" ]
                    ]
                , misc: \str -> [ HH.text str ]
                }
            )
        ]

-- TODO: keep track of section section_depth and section_index
-- TODO: make collapseable (should start collapsed, actually)
renderTableOfContents :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML
renderTableOfContents doc =
  do
    let
      tree :: Tree _
      tree = doc # Fix.fold case _ of
        Page _opts prms body -> Branch { id: prms.id, title: prms.title } body
        Section _opts prms body -> Branch { id: prms.id, title: prms.title } body
        _ -> Leaf

      prune :: Tree _ -> Tree _
      prune Leaf = Leaf
      prune (Branch a kids) = Branch a $ kids # map prune # Array.filter (not <<< isLeaf)

      renderNode node =
        HH.div_
          [ HH.a
              [ HP.href $ "#" <> node.id ]
              [ HH.text $ "• " <> node.title ]
          ]

      go :: Tree _ -> Array HTML
      go Leaf = []
      go (Branch node kids) | null kids =
        [ renderNode node ]
      go (Branch node kids_) =
        let
          kids = kids_ # map go
        in
          [ HH.div
              [ Style.css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
              [ renderNode node
              , HH.div
                  [ Style.css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em", "padding-left: 1em" ] ]
                  (kids # fold)
              ]
          ]

    pure
      $ HH.div
          [ Style.css do
              tell [ "padding: 1em", "box-shadow: 0 0 0 1px black" ]
              tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
          ]
      $ fold
          [ [ HH.div
                [ Style.css do tell [ "font-size: 1.5em" ] ]
                [ HH.text "Table of Contents" ]
            ]
          , go $ prune $ tree
          , [ HH.div []
                [ HH.a
                    [ HP.href "#bibliography" ]
                    [ HH.text "• Bibliography" ]
                ]
            ]
          ]

renderSectionTitle :: forall m. Monad m => _ -> m HTML
renderSectionTitle { section_depth, section_index, id, title } =
  pure $
    HH.div
      [ Style.css $ tell
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
          [ Style.css $ tell [ "flex-grow: 0" ] ] $
          [ HH.a
              [ HP.href $ "#" <> id ]
              [ HH.text $ "§" ]
          , title
          ]
      , HH.div
          [ Style.css $ tell [ "flex-shrink: 0", "flex-grow: 1", "text-align: right" ] ] $ fold $
          [ section_index # maybe [] \str -> [ HH.text str ]
          , [ HH.a
                [ HP.href $ "#" <> id ]
                [ HH.text $ "§" ]
            ]
          ]
      ]

-- TODO: add something extra about citation.time and citation.note
-- TODO: this is probably combined with renderCitation
renderCitation :: forall m. MonadReader Ctx m => MonadState Env m => Citation -> m HTML
renderCitation citation_ = do
  citation <- citation_ # renderCitation
  pure $
    HH.div
      [ Style.css $ do
          tell
            [ "padding-left: 40%"
            , "padding-right: 10%"
            , "display: flex"
            , "flex-direction: column"
            , "align-items: flex-end"
            ]
      ]
      [ HH.div
          [ Style.css $ do
              tell
                [ "padding: 0.5em"
                , "background-color: color-mix(in hsl, saddlebrown, transparent 80%)"
                ]
          ]
          [ citation ]
      ]

renderBibliography :: forall m. MonadReader Ctx m => MonadState Env m => Doc -> m HTML
renderBibliography doc = do
  -- TODO: handle citations and resources
  let citations_ = RL.collectCitations doc
  citations <- citations_ # traverse \citation -> do
    html <- renderCitation citation
    pure $
      HH.div
        [ Style.css do tell [ "padding: 0.5em", "background-color: color-mix(in hsl, saddlebrown, transparent 80%)" ] ]
        [ html ]
  title <-
    renderSectionTitle
      { section_depth: 0
      , section_index: none
      , title: HH.text "Bibliography"
      , id: "bibliography"
      }
  pure $
    HH.div
      [ HP.id "bibliography"
      , Style.css do
          tell [ "padding: 1em", "box-shadow: 0 0 0 1px black" ]
          tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
      ]
      [ title
      , HH.div
          [ Style.css do tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ] ]
          citations
      ]

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
      -- bgcolor = "rgba(0, 0, 0, 0.1)"
      bgcolor = "color-mix(in hsl, indigo, transparent 90%)"
    in
      HH.div
        [ Style.css $ tell [ "display: inline" ] ] $ fold $
        [ [ HH.div
              [ Style.css $ tell [ "display: inline", "user-select: none", "cursor: pointer", "background-color: " <> bgcolor, "padding-left: 0.3em" ]
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
                [ Style.css $ tell
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
              [ Style.css $ tell
                  [ "display: inline"
                  , "user-select: none"
                  , "cursor: pointer"
                  , "background-color: " <> bgcolor
                  , "padding-right: 0.3em"
                  ]
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
--       , Style.css $ tell [ "box-shadow: 0 0 0 0.1em black inset", "display: flex", "flex-direction: column", "padding: 0.5em", "gap: 0.5em" ]
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

