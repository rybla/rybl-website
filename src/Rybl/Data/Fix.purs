module Rybl.Data.Fix where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Traversable (class Traversable, traverse, traverse_)
import Rybl.Utility (bind')

data Fix f = Fix (f (Fix f))

derive instance Generic (Fix f) _

instance (Functor f, Show (f String)) => Show (Fix f) where
  show = fold show

-- instance Eq1 f => Eq (Fix f) where
--   eq 

-- instance Ord1 f => Ord (Fix f) where
--   compare x y = genericCompare x y

instance (Traversable f, EncodeJson (f Json)) => EncodeJson (Fix f) where
  encodeJson = fold encodeJson

instance (Traversable f, DecodeJson (f Json)) => DecodeJson (Fix f) where
  decodeJson = unfoldM decodeJson

wrap :: forall f. f (Fix f) -> Fix f
wrap = Fix

unwrap :: forall f. Fix f -> f (Fix f)
unwrap (Fix x) = x

fold :: forall f a. Functor f => (f a -> a) -> Fix f -> a
fold f (Fix x) = x # map (fold f) # f

foldM :: forall f m a. Traversable f => Monad m => (f a -> m a) -> Fix f -> m a
foldM f = unwrap >>> traverse (foldM f) >>> bind' f

unfoldM :: forall f m a. Traversable f => Monad m => (a -> m (f a)) -> a -> m (Fix f)
unfoldM f = f >=> traverse (unfoldM f) >>> map wrap

traverse_upwards
  :: forall f m
   . Traversable f
  => Monad m
  => (f (Fix f) -> m (f (Fix f)))
  -> Fix f
  -> m (Fix f)
traverse_upwards f =
  unwrap
    >>> traverse (traverse_upwards f)
    >>> bind' (f >=> (wrap >>> pure))

traverse_upwards_
  :: forall f m
   . Traversable f
  => Monad m
  => (f (Fix f) -> m Unit)
  -> Fix f
  -> m Unit
traverse_upwards_ f (Fix x) = do
  x # traverse_ (traverse_upwards_ f)
  f x

traverse_downwards
  :: forall f m
   . Traversable f
  => Monad m
  => (f (Fix f) -> m (f (Fix f)))
  -> Fix f
  -> m (Fix f)
traverse_downwards f =
  unwrap
    >>> f
    >>> bind' (traverse (traverse_downwards f) >=> (wrap >>> pure))

traverse_downwards_
  :: forall f m
   . Traversable f
  => Monad m
  => (f (Fix f) -> m Unit)
  -> Fix f
  -> m Unit
traverse_downwards_ f (Fix x) = do
  f x
  x # traverse_ (traverse_downwards_ f)
