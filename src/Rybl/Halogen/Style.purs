module Rybl.Halogen.Style where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Int as Int
import Data.List (List)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Rybl.Data.Variant (reflectVariantKey)
import Rybl.Utility (class RowKeys, Literal(..), literal, rowKeys)
import Type.Proxy (Proxy(..))

type U = Unit

type Style = StyleM U
type StyleM = Writer (Array String)

style :: forall r i. Style -> IProp (style :: String | r) i
style sty = HP.style $ execWriter sty # Array.intercalate "; "

class Render a where
  render :: a -> String

class RenderStyle a where
  renderStyle :: a -> Array String

type RenderStyleF a = a -> Array String
type RenderF a = a -> String

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

malformed :: forall a. String -> String -> a
malformed label v = unsafeCrashWith $ "malformed " <> label <> ": " <> v

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

instance Render Number where
  render = show

instance Render String where
  render = show

instance Render Int where
  render = show

instance Render (Literal xs) where
  render (Literal v) = reflectVariantKey v

newtype Name = Name String

instance Render Name where
  render (Name s) = s

newtype PosInt = PosInt Int

instance Render PosInt where
  render (PosInt x) = show x

newtype Percent = Percent Int

instance Render Percent where
  render (Percent x) = show x <> "%"

newtype Ratio = Ratio Number

instance Render Ratio where
  render (Ratio x) = show x

data Hex2 = Hex2 Int

instance Render Hex2 where
  render (Hex2 n) = Int.toStringAs Int.hexadecimal n

data Hex2x3 = Hex2x3 Hex2 Hex2 Hex2

instance Render Hex2x3 where
  render (Hex2x3 n1 n2 n3) = render n1 <> render n2 <> render n3

data Measure a u = Measure a (Literal u)

type Time = Measure Number ("s" :: U, "ms" :: U)

instance Render a => Render (Measure a u) where
  render (Measure a u) = render a <> render u

--------------------------------------------------------------------------------
-- Color
--------------------------------------------------------------------------------

data Color = Color PosInt PosInt PosInt Ratio

instance Render Color where
  render (Color r g b a) = neutral "rgba" [ render r, render g, render b, render a ]

--------------------------------------------------------------------------------
-- Cast
--------------------------------------------------------------------------------

class Cast a b where
  cast :: a -> b

cast' :: forall @b a. Cast a b => a -> b
cast' = cast

instance Cast Int Number where
  cast = Int.toNumber

instance Cast Number Int where
  cast = Int.round

instance Cast String Name where
  cast = Name -- TODO: validate

instance Cast Int PosInt where
  cast i | i < 0 = malformed "PosInt" $ show i
  cast i = PosInt i

instance Cast Percent Ratio where
  cast (Percent n) = Ratio $ cast n / 100.0

instance Cast Ratio Percent where
  cast (Ratio x) = Percent $ cast $ x * 100.0

{-
--------------------------------------------------------------------------------
-- CSS Properties
--------------------------------------------------------------------------------

accent_color :: Color -> Style
accent_color c = tell [ "accent-color: " <> render c ]

align_content :: Literal ("stretch" :: U, "center" :: U, "flex-start" :: U, "flex-end" :: U, "space-between" :: U, "space-around" :: U, "space-evenly" :: U, "initial" :: U, "inherit" :: U) -> Style
align_content x = tell [ "align-content: " <> (x # ?a) ]

align_items :: Literal ("normal" :: U, "stretch" :: U, "center" :: U, "flex-start" :: U, "flex-end" :: U, "start" :: U, "end" :: U, "baseline" :: U, "initial" :: U, "inherit" :: U) -> Style
align_items x = tell [ "align-items: " <> (x # ?a) ]

align_self :: Literal ("auto" :: U, "self" :: U, "stretch" :: U, "center" :: U, "flex-start" :: U, "flex-end" :: U, "baseline" :: U, "initial" :: U, "inherit" :: U) -> Style
align_self x = tell [ "align-self: " <> (x # ?a) ]

type AnimationArgs = AnimationArgs'
  (name :: Name)

type AnimationArgs' r =
  ( duration :: Time
  , timing_function :: Literal ("linear" :: U, "ease" :: U, "ease-in" :: U, "ease-out" :: U, "ease-in-out" :: U, "step-start" :: U, "step-end" :: U, "initial" :: U, "inherit" :: U)
  , delay :: Time
  , iteration_count :: Literal ("infinite" :: U, "initial" :: U, "inherit" :: U)
  , direction :: Literal ("normal" :: U, "reverse" :: U, "alternate" :: U, "alternate-reverse" :: U, "initial" :: U, "inherit" :: U)
  , fill_mode :: Literal ("none" :: U, "forwards" :: U, "backwards" :: U, "both" :: U, "initial" :: U, "inherit" :: U)
  , play_state :: Literal ("paused" :: U, "running" :: U, "initial" :: U, "inherit" :: U)
  | r
  )

animation :: Record AnimationArgs -> Style
animation { name, duration, timing_function, delay, iteration_count, direction, fill_mode, play_state } = tell [ "animation: " <> Array.intercalate " " [ render name, render duration, render timing_function, render delay, render iteration_count, render direction, render fill_mode, render play_state ] ]

animation' :: forall r r'. Union r (AnimationArgs' ()) r' => Nub r' AnimationArgs => Record r -> Style
animation' args = animation
  ( Record.merge args
      ( { timing_function: literal @"linear"
        , delay: Measure 0.0 (literal @"s")
        , duration: Measure 1.0 (literal @"s")
        , iteration_count: literal @"infinite"
        , direction: literal @"normal"
        , fill_mode: literal @"none"
        , play_state: literal @"running"
        } :: Record (AnimationArgs' ())
      )
  )

-- background
-- border
-- box_shadow
-- color
-- cursor
-- display
-- flex
-- flex_direction
-- flex_flow
-- flex_wrap
-- float
-- font
-- font_size
-- font_weight
-- gap
-- height
-- justify_content
-- justify_items
-- line_height
-- left
-- margin
-- max_height
-- max_width
-- min_height
-- min_width
-- opacity
-- outline
-- overflow
-- overflow_x
-- overflow_y
-- padding
-- position
-- right
-- text_align
-- text_decoration
-- text_decoration_color
-- text_decoration_line
-- text_decoration_style
-- text_decoration_thickness
-- text_emphasis
-- text_orientation
-- text_shadow
-- text_transform
-- top
-- user_select
-- vertical_align
-- visibility
-- white_space
-- width
-- word_break
-- word_spacing
-- word_wrap
-- writing_mode
-- z_index
-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

neutral :: String -> Array String -> String
neutral f as = f <> "(" <> Array.intercalate ", " as <> ")"
