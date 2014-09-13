module Main where

import Functors (p)

main::IO()
main = do
  print $ fmap (+ 5) p
  print "hi"