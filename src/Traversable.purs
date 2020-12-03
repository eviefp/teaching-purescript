module Traversable where

import Prelude

import Data.Either (Either(..))
import Data.Foldable as F
import Data.Lens.Lens.Product (_1)
import Data.List (List(..))
import Data.Maybe (Maybe(..))

-- TODO: Learn about function instances next time and solve this!
class Foldable f where
    foldMap :: forall a m. Monoid m => (a -> m) -> f a -> m
    -- foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
    -- foldl :: forall a b. (b -> a -> b) -> b -> f a -> b

{-
foldr (+) 0 [1, 2, 3, 4]

1 + (2 + (3 + (4 + 0)))

----------------------------------------------

foldl (+) 0 [1, 2, 3, 4]

(((0 + 1) + 2) + 3) + 4

-}

instance maybeFoldable :: Foldable Maybe where
    foldMap a2m maybeA = case maybeA of
        Nothing -> mempty
        Just a  -> a2m a

instance arrayFoldable :: Foldable Array where
    foldMap a2m arrA = F.foldr (\a m -> a2m a <> m) mempty arrA

instance listFoldable :: Foldable List where
    foldMap a2m listA = case listA of
        Nil -> mempty
        Cons a as -> a2m a <> foldMap a2m as

instance eitherFoldable :: Foldable (Either e) where
    -- a2m :: a -> m
    -- eitherEA :: Either e a
    foldMap a2m eitherEA = case eitherEA of
        Left e -> mempty
        Right a -> a2m a

class (Functor t, Foldable t) <= Traversable t where
    traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
    sequence :: forall a m. Applicative m => t (m a) -> m (t a)
    -- f  :: a -> m b
    -- ta :: t a
    -- f <$> ta :: t (m b)

instance maybeTraversable :: Traversable Maybe where
    traverse :: forall a b m. Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
    traverse f maybeA =  case maybeA of
        Nothing -> pure Nothing 
        Just a -> Just <$> f a
    sequence = sequenceDefault

instance listTraversable :: Traversable List where
    traverse :: forall a b m. Applicative m => (a -> m b) -> List a -> m (List b)
    traverse f listA =  case listA of
        Nil -> pure Nil
        Cons a as -> Cons <$> f a <*> traverse f as
    sequence = sequenceDefault

instance eitherTraversable :: Traversable (Either e) where
    traverse :: forall a b m. Applicative m => (a -> m b) -> Either e a -> m (Either e b)
    traverse f eitherEA =  case eitherEA of
            Left e -> pure (Left e)
            Right a -> Right <$> f a
    sequence = sequenceDefault
    
for :: forall a b m t. Traversable t => Applicative m => t a -> (a -> m b) -> m (t b)
for it f = traverse f it

-- identity :: forall a. a -> a
-- identity a = a

{-

traverse :: (x -> n y) -> f x -> n (f y)

tma :: t (m a)
tma :: f x

x = m a

traverse _ tma :: (m a -> n y) -> f (m a) -> n (f y)

n (f y) = m (t a)
n = m
f = t
y = a

traverse _ tma :: (m a -> m a) -> t (m a) -> m (t a)
-}
sequenceDefault :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceDefault tma = traverse identity tma

{-
sequence :: t (m a) -> m (t a)
a = b

f :: (a -> m b)
ta :: t a

f <$> ta :: t (m b)
sequence :: t (m b) -> m (t b)
-}
traverseDefault :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseDefault f ta = sequence $ f <$> ta
