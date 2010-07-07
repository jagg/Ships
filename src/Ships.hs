{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ships  where 

import IA

import Data.Array(Array(..),(!),bounds,elems,indices,ixmap,listArray,(//))
import Data.Ix (Ix(..))
import Data.Maybe
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (liftIO)

data Cell = Water
            | WaterHit
            | Ship  
            | Fire  deriving (Eq, Show)
                             
data Player = One                             
              | Two deriving (Eq, Show)

type Board = Array (Int,Int) Cell

data Game = Game {
    board1  :: Board,
    board2  :: Board,
    points1 :: Int,
    points2 :: Int,
    mov     :: Int
  }
            
data GameConf = GameConf {            
    shipsNumber :: Int
  }
                
data LogEntry = LogEntry {
    player     :: Player,
    movNum     :: Int,
    movement   :: (Int,Int)
  } deriving (Show)
            
instance Show Game where
  show g = "<Game>\n" ++ showBoard (board1 g) True ++ " --***-- \n" ++ showBoard (board2 g) False

-- I'll probably need the IO monad at the end of the stack, to show the results without
-- leaving the GameMonad...
newtype GameMonad a = GM {
  runGame :: ReaderT GameConf (WriterT [LogEntry] (StateT Game IO)) a
  }deriving (Monad,MonadIO, MonadReader GameConf, MonadWriter [LogEntry], MonadState Game)


--The idea is that "a" will be the winner (0,1,2)?
runApp :: GameMonad a -> Int -> IO ((a,[LogEntry]), Game)
runApp m sn = 
  let config = GameConf sn
      createBoard as =listArray ((0,0),(2,2)) as  
      state  = Game (createBoard [Water,Water,Ship,Water,Ship,Ship,Water,Water,Ship]) 
                    (createBoard [Water,Ship,Ship,Water,Ship,Ship,Water,Water,Water]) 0 0 0
  in runStateT (runWriterT (runReaderT (runGame m) config)) state


transform :: Cell -> String
transform Water = "-"
transform WaterHit = "o"
transform Ship = "s"
transform Fire = "x"

hide :: Bool -> Cell -> Cell
hide True Ship = Water
hide _ c = c

-- TODO make a tail recursive function and iterate on the array size adding a new line at the end of each row.
showBoard :: Board -> Bool -> String
showBoard b h = sbRec 0 0 ""
  where symbols = listArray (0,size-1) $  map (transform . (hide h)) $ elems b
        ((y0,y),(x0,x))   = bounds b
        size              = (x0-y0+1) * (x-y+1) 
        sbRec acc tot s = if (acc < x && tot < size) then  sbRec (acc+1) (tot+1) (s ++ symbols!tot)
                          else if (acc == x && tot <= size) then sbRec 0 (tot+1) (s ++ symbols!tot ++ "\n")
                               else if tot >= size then s
                                    else s ++ "-ouch-"               

                                                      
printGame :: Game -> IO()
printGame g = putStr $ show g

checkGame' :: GameMonad Int
checkGame' = do
  st       <- get
  conf     <- ask
  return $ selectWinner (board1 st) (board2 st)
  where selectWinner b1 b2 = 
          if foldr accShip Water (elems b1) /= Ship then 2  
          else if foldr accShip Water (elems b2) /= Ship then 1         
               else 0                       
        accShip Ship _ = Ship            
        accShip _ Ship = Ship
        accShip _ _    = Water

        
shot :: Array (Int,Int) Cell -> Int -> Int -> Array (Int, Int) Cell
shot a x y = case a!(x,y) of
  Water -> a // [((x,y), WaterHit)]
  Ship  -> a // [((x,y), Fire)]
  _     -> a

move :: Player -> (Int,Int) -> GameMonad Int  
move p (x,y) = get >>= \state ->
       ( if p == One then  put state { board2 = shot (board2 state) x y, mov = (mov state) + 1 }
       else  put state { board1 = shot (board1 state) x y, mov = (mov state) + 1  })
       >> tell [LogEntry p (mov state) (x,y)] >> checkGame' 

                              
askMove :: GameMonad (Int,Int)                              
askMove = 
  do  liftIO (putStrLn "Horizontal value")
      y <- liftIO getLine
      liftIO (putStrLn "Vertical value")
      x <- liftIO getLine
      return (read x,read y)
      
display :: GameMonad ()
display = do
  state <- get
  liftIO $ printGame state

-- Player vs Player
play :: GameMonad Int      
play = display >> askMove >>= (move One) >> askMove >>= (move Two) >>= \s ->
       if s == 0 then play
                 else return s   
                      
-- Player vs Computer
playComp :: GameMonad Int      
playComp = display >> liftIO (randomMove 2 2) >>= (move One) >> askMove >>= (move Two) >>= \s ->
       if s == 0 then playComp
                 else return s   


test :: IO()
test = do 
  ((result,_),g) <- runApp playComp 3
  printGame g
  print $ "The winner is " ++ (show result)

  
