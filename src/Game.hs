module Main where
    
import qualified Data.Array.Repa as R (fromFunction,computeS,sumAllS,extract)
import Data.Array.Repa 

import Control.Monad (forM_)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
    
-- Types
type State = Int

showState :: State -> Char
showState 1 = 'O'
showState 0 = ' '
    
type Board = Array U DIM2 State

type Index = (Int,Int)

type Object = [Index]

n :: Int
n = 10

alive :: Int
alive = 1

dead :: Int
dead = 0

-- Functions
initBoard :: [Index] -> Board
initBoard xs = R.computeS $ R.fromFunction (Z :. n :. n) fun
    where
        fun (Z :. i :. j) = if (i,j) `elem` xs then alive
                            else dead

printBoard :: Board -> IO ()
printBoard board = printHead >> g0 1 1
    where
        printHead = putChar '+' >> forM_ (replicate (2*n-2) '-') putChar >> putChar '+' >> putChar '\n'
        printBar  = putChar '|'
        printLine = putChar '\n' >> forM_ (replicate (2*n-1) '-') putChar >> putChar '\n'
        
        g0 i j
            | j == n && i == n-1 = printBar >> putChar '\n' >> printHead
            | j == n = printBar >> printLine >> g0 (i+1) 1
            | otherwise = printBar >> putChar (showState $ board ! (Z :. i :. j)) >> g0 i (j+1)

nextState :: Board -> DIM2 -> State
nextState board ix@(Z :. i :. j) 
    | i == 0 || i == (n-1) || j == 0 || j == (n-1) = dead
    | otherwise =
        let sumM = R.sumAllS $ R.extract (Z :. (i-1) :. (j-1)) (Z :. 3 :. 3) board 
        in case board ! ix of
                1 -> if sumM == 3 || sumM == 4 then alive else dead
                0 -> if sumM == 3 then alive else dead
                             
stepBoard :: Board -> Board
stepBoard board = R.computeS $ R.fromFunction (Z :. n :. n) (nextState board)

blinker :: Object 
blinker = [(3,3),(3,4),(3,5)] 

loop :: Board -> Int -> Int -> IO ()
loop board 0 _ = putStrLn "END OF GAME"
loop board n t = do
    printBoard board
    threadDelay t
    putChar '\n'
    loop (stepBoard board) (n-1) t
     
main :: IO ()
main = do
    [delay] <- getArgs
    let init = initBoard blinker
    loop init 10 (read delay * 1000)
        
    
    
    
    