module IA where

import System.Random

randInt :: Int -> IO Int
randInt l = getStdRandom (randomR (0, l))

randomMove :: Int -> Int -> IO (Int,Int)
randomMove mX mY = do
  x <- randInt mX
  y <- randInt mY
  return (x,y)
  
generateMoves :: Int -> Int -> [(Int,Int)]  
generateMoves mx my = [(x,y) | x<- [0..mx], y <- [0..my]]

