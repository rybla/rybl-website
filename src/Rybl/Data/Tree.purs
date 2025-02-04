module Rybl.Data.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)

data Tree a = Branch a (Array (Tree a)) | Leaf

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x y = genericEq x y

derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

isLeaf :: forall a. Tree a -> Boolean
isLeaf Leaf = true
isLeaf _ = false

