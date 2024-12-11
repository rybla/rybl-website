module Rybl.Utility where

import Prelude

import Data.Function (applyFlipped)
import Data.Lens.Record as Data.Lens.Record
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Record as Record
import Type.Prelude (Proxy(..))

infixl 0 applyFlipped as ##

todo :: forall b3. String -> b3
todo msg = unsafeCrashWith $ "TODO: " <> msg

prop' :: forall @l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => (forall p. Strong p => p a b -> p (Record r1) (Record r2))
prop' = Data.Lens.Record.prop (Proxy @l)

insert' :: forall @l r1 r2 a. IsSymbol l => Lacks l r1 => Cons l a r1 r2 => a -> Record r1 -> Record r2
insert' = Record.insert (Proxy @l)