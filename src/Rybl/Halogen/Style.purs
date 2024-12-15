module Rybl.Halogen.Style where

import Prelude

import Control.Monad.Writer (Writer, execWriter)
import Data.Array as Array
import Data.Int as Int
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Rybl.Data.Variant (Variant)

type Style = StyleM Unit
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

malformed :: forall a. String -> String -> a
malformed label v = unsafeCrashWith $ "malformed " <> label <> ": " <> v

--------------------------------------------------------------------------------

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

-- instance Cast Name Color where
--   cast name = BaseColor (NamedBaseColor name)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

neutral :: String -> Array String -> String
neutral f as = f <> "(" <> Array.intercalate ", " as <> ")"