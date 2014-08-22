module Functors where

import Prelude hiding (Either, Left, Right)
-- Implement Functor instances

-- Either
data Either a b = Left a | Right b

instance Functor (Either e) where
  fmap _ (Left a) = Left a
  fmap g (Right b) = Right (g b)
   
-- Pair
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
  fmap g (Pair a b) = Pair (g a) (g b)
  
p :: Pair Integer
p = Pair 1 2

-- Tree
data ITree a = Leaf (Int -> a) 
           | Node [ITree a]
           
instance Functor ITree where
  fmap g (Leaf f) = Leaf $ g . f
  fmap g (Node children) = Node $ map (fmap g) children 
           
-- Reader
newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
  fmap g (Reader r) = Reader { runReader = g . r }