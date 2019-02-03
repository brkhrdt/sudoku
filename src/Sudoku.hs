module Sudoku where

import Data.Array
import Data.Char  (digitToInt)
import Data.List  (intersect)
import Debug.Trace
import Control.Monad
import System.IO

--import Data.Array.Repa
--type Puzzle = Array U DIM2 Int
--let x :: Puzzle; x = fromListUnboxed (Z :. (9 :: Int) :. (9 :: Int)) ([1..81] :: [Int])

-- Board
-- (0,0)...........(0,8)
-- .                 .
-- .                 .
-- .      (4,4)      .
-- .                 .
-- .                 .
-- (8,0)...........(8,8)

type Coord = (Int, Int)
type Board = Array Coord Int

-- For project euler #96
getTopLeftNum :: Board -> Int
getTopLeftNum b = 100*(b ! (0,0)) + 10*(b ! (0,1)) + (b ! (0,2))

solver :: Board -> [Board]
solver b = if validBoard b then [b]
           else foldr (\x xs -> if validBoard x
                                  then (x:xs)
                                  else xs
                      ) [] (concat $ map solver $ possibleBoards (nextEmpty b) b)

allCoords :: [Coord]
allCoords = [(i,j) | i <- [0..8], j <- [0..8]]

validBoard :: Board -> Bool
validBoard b = all (\c -> null $ candidates c b) allCoords

nextEmpty :: Board -> Coord
nextEmpty b = next $ assocs b
  where next [] = error ("shouldn't have any nonempty: " ++ boardToGrid b)
        next ((c,v):xs) = if v == 0 then c else next xs

-- Fill coord with all possible numbers and return those boards
possibleBoards :: Coord -> Board -> [Board]
possibleBoards c b = if (b ! c) == 0
                       then map (\v -> b // [(c,v)]) (candidates c b)
                       else [b]

candidates :: Coord -> Board -> [Int]
candidates c b = vertCandidates c b `intersect` horizCandidates c b `intersect` boxCandidates c b

vertCandidates :: Coord -> Board -> [Int]
vertCandidates (_,j) b = filterCandidates b [(x,j) | x <- [0..8]]

horizCandidates :: Coord -> Board -> [Int]
horizCandidates (i,_) b = filterCandidates b [(i,y) | y <- [0..8]]

boxCandidates :: Coord -> Board -> [Int]
boxCandidates c b = let (i,j) = getNonantCenter $ getNonant c
                    in filterCandidates b [(x,y) | x <- [i-1,i,i+1], y <- [j-1,j,j+1]]

-- Get list of Ints not present at the given coordinates
filterCandidates :: Board -> [Coord] -> [Int]
filterCandidates b cs = foldr (\(x,y) vals ->
                           if b ! (x,y) == 0 then vals
                           else filter (noMatch (b ! (x,y))) vals) [1..9] cs
                             where noMatch c = (\x -> x /= c)


getNonantCenter :: Int -> Coord
getNonantCenter n
  | n == 1 = (1,1)
  | n == 2 = (1,4)
  | n == 3 = (1,7)
  | n == 4 = (4,1)
  | n == 5 = (4,4)
  | n == 6 = (4,7)
  | n == 7 = (7,1)
  | n == 8 = (7,4)
  | n == 9 = (7,7)
  | otherwise = error ("nonant must be 1-9: " ++ show n)

-- Nonants: (let's just assume this means "quadrants" for nine parts)
-- 1 2 3
-- 4 5 6
-- 7 8 9
getNonant :: Coord -> Int
getNonant (i,j) = ((rowNonants i) `intersect` (colNonants j)) !! 0
  where rowNonants :: Int -> [Int]
        rowNonants x
            | 0 <= x && x <= 2 = [1,2,3]
            | 3 <= x && x <= 5 = [4,5,6]
            | 6 <= x && x <= 8 = [7,8,9]
            | otherwise = error "no nonant?"
        colNonants :: Int -> [Int]
        colNonants y
            | 0 <= y && y <= 2 = [1,4,7]
            | 3 <= y && y <= 5 = [2,5,8]
            | 6 <= y && y <= 8 = [3,6,9]
            | otherwise = error "no nonant?"




--
-- Printing Boards
--
stringToBoard :: String -> Board
stringToBoard s = array ((0,0),(8,8)) $ zip coords ints
  where ints = map digitToInt s
        coords = [(i,j) | i <- [0..8], j <- [0..8]]

boardToString :: Board -> String
boardToString b = concat $ map printCell (elems b)

-- 9x9 format with newlines
boardToGrid :: Board -> String
boardToGrid b = insertNewlines $ boardToString b
  where insertNewlines [] = []
        insertNewlines xs = let (line, rest) = (splitAt 9 xs)
                            in line ++ "\n" ++ insertNewlines rest

-- Return string represenation of cell
printCell :: Int -> String
printCell x
  | 1 <= x && x <= 9 = show x
  | x == 0 = "."
  | otherwise = error $ "Unexpected board cell value: " ++ show x

--
-- Board Files
--
readBoardsFromFile :: FilePath -> IO [(String, Board)]
readBoardsFromFile f = do
  fh <- openFile f ReadMode
  boards <- readBoardsFH fh
  return boards

readBoardsFH :: Handle -> IO [(String, Board)]
readBoardsFH fh = do
  atEOF <- hIsEOF fh
  if atEOF
    then return []
    else do
      boardNum <- hGetLine fh
      bstr <- ((filter (/= '\n')) . unlines) <$> (replicateM 9 (hGetLine fh))
      moreBoards <- readBoardsFH fh
      return $ (boardNum, stringToBoard bstr) : moreBoards
