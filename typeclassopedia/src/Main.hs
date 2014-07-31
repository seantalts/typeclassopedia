module Main where

import Prelude hiding (Either, Left, Right)
-- Implement Functor instances for Either e and ((->) e).

data Either a b = Left a | Right b

instance Functor (Either e) where
  fmap _ (Left a) = Left a
  fmap g (Right b) = Right (g b)
   
instance Functor ((->) e) where
  fmap g 

main::IO()
main = undefined