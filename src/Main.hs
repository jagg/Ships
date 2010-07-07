module Main where

import Ships

main :: IO()
main = do 
  ((result,_),g) <- runApp playComp 3
  printGame g
  print $ "The winner is " ++ (show result)