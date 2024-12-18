module Rybl.Utility where

import Prelude

import Data.Foldable (class Foldable)
import Data.Function (applyFlipped)
import Data.Lens.Record as Data.Lens.Record
import Data.List (List, (:))
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (class Traversable, traverse, traverse_)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Rybl.Data.Variant (Variant, inj', reflectVariantKey)
import Type.Prelude (Proxy(..))

type U = Unit

infixl 0 applyFlipped as ##

todo :: forall b3. String -> b3
todo msg = unsafeCrashWith $ "TODO: " <> msg

prop' :: forall @l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => (forall p. Strong p => p a b -> p (Record r1) (Record r2))
prop' = Data.Lens.Record.prop (Proxy @l)

insert' :: forall @l r1 r2 a. IsSymbol l => Lacks l r1 => Cons l a r1 r2 => a -> Record r1 -> Record r2
insert' = Record.insert (Proxy @l)

infixl 1 traverse as $@=
infixl 1 traverse_ as $@

traverseFlipped :: forall t a f b. Traversable t => Applicative f => t a -> (a -> f b) -> f (t b)
traverseFlipped = flip traverse

infixr 1 traverseFlipped as =@$

traverseFlipped_ :: forall t a f b. Applicative t => Foldable f => f a -> (a -> t b) -> t Unit
traverseFlipped_ = flip traverse_

infixr 1 traverseFlipped_ as @$

class RowKeys :: forall k. Row k -> Constraint
class RowKeys r where
  rowKeys :: List String

instance (RowToList r rl, RowKeys_RL rl) => RowKeys r where
  rowKeys = rowKeys_RL @rl

class RowKeys_RL :: forall k. RowList k -> Constraint
class RowKeys_RL rl where
  rowKeys_RL :: List String

instance RowKeys_RL RL.Nil where
  rowKeys_RL = mempty

instance (IsSymbol x, RowKeys_RL rl) => RowKeys_RL (RL.Cons k v rl) where
  rowKeys_RL = reflectSymbol (Proxy @x) : rowKeys_RL @rl

newtype Literal xs = Literal (Variant xs)

instance Show (Literal xs) where
  show (Literal l) = reflectVariantKey l

instance Eq (Literal xs) where
  eq (Literal l1) (Literal l2) = reflectVariantKey l1 == reflectVariantKey l2

instance Ord (Literal xs) where
  compare (Literal l1) (Literal l2) = compare (reflectVariantKey l1) (reflectVariantKey l2)

literal :: forall @x xs_ xs. IsSymbol x => Cons x Unit xs_ xs => Literal xs
literal = Literal (inj' @x unit)

