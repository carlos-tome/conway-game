module Game where
    
import qualified Data.Array.Repa as R (fromFunction,computeS)
import Data.Array.Repa 

import Control.Monad (forM_)
    
-- Types
type State = Bool

showState :: State -> Char
showState True  = 'O'
showState False = ' '
    
type Board = Array U DIM2 State

type Index = (Int,Int)

n :: Int
n = 10

-- Functions
initBoard :: [Index] -> Board
initBoard xs = R.computeS $ R.fromFunction (Z :. n :. n) fun
    where
        fun (Z :. i :. j) =  (i,j) `elem` xs 

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
                
step :: Board -> Board
step board = R.computeS $ R.fromFunction (Z :. n :. n) fun
    where
    
    
                
main :: IO ()
main = undefined