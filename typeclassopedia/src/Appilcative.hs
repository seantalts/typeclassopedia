module Appilcative where

import Control.Applicative hiding (ZipList, getZipList)
import Prelude hiding (Maybe)

--Determine the correct definition of pure for the ZipList instance of 
--Applicative—there is only one implementation that satisfies the law relating pure and (<*>). 

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap g zl = ZipList (map g (getZipList zl))
 
instance Applicative ZipList where
  pure a = ZipList $ repeat a
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
  
-- Implement an instance of Applicative for Maybe.
data Maybe a = Some a | None

instance Functor Maybe where
  fmap g (Some x) = Some (g x)
  fmap _ None = None

instance Applicative Maybe where
  pure = Some
  Some f <*> Some x = Some $ f x
  None <*> _ = None
  Some _ <*> None = None
  
--Implement pure and (<*>) in terms of unit and (**), and vice versa.
class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = pure ()
  (**) = undefined
