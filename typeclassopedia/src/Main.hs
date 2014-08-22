module Main where

import Functors (p)

main::IO()
main = print $ fmap (+ 7) p
