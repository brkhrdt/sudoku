module Main where

import Sudoku

import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Monad
import Debug.Trace
import Data.Array

-- Opening tests in repl:
-- > cabal new-repl test:tests

--
-- QUICKCHECK
--
m_QC_MAX_EMPTY_CELLS = 50

genCoord :: Gen Coord
genCoord = (arbitrary :: Gen (Int, Int))
           `suchThat` (\(x,y) -> (x >= 0 && x <= 8)
                              && (y >= 0 && y <= 8))

fillBoard :: [Coord] -> Board -> Maybe Board
fillBoard [] board = Just board
fillBoard (c:cs) board = case possibleBoards c board of
                  [] -> Nothing
                  posBoards -> case filter isJust (map (fillBoard cs) posBoards) of
                                 [] -> Nothing
                                 (b:bs) -> b

genCoords :: Gen [Coord]
genCoords = shuffle allCoords

genBoard :: Gen Board
genBoard = do
  bs <- genBoards allCoords emptyBoard
  case (bs) of
    [] -> error "no board from generator"
    (b:_) -> return b

genBoards :: [Coord] -> Board -> Gen [Board]
genBoards [] board  = return $ [(traceBoard  board)]
genBoards coords board = do
  (c:cs) <- return coords
  case possibleBoards c (traceBoard board) of
    [] -> return []
    pbs -> do pbsShuffled <- shuffle pbs
              mbs <- sequence $ map (genBoards cs) pbsShuffled
              let bs = concat mbs
              return $ bs

genUnsolved :: Board -> Gen Board
genUnsolved bs = do
  n <- choose (1, m_QC_MAX_EMPTY_CELLS)
  cs <- take n <$> genCoords
  let b = foldr (\c b -> b // [(c,0)]) bs cs
  return (traceBoard b)

emptyBoard :: Board
emptyBoard = stringToBoard (replicate 81 '0')

prop_solve :: Board -> Board -> Bool
prop_solve bSolved bUnsolved = bSolved `elem` (solver bUnsolved)


--
-- HSPEC
--

-- Output: (Name, Problem Board, Solution Board)
readTestcasesFromFile :: FilePath -> IO [(String, Board, Board)]
readTestcasesFromFile f = do
  content <- readFile f
  ls <- return $ lines content
  bs <- return $ map parseTestcase ls
  return bs

parseTestcase :: String -> (String, Board, Board)
parseTestcase = (\l -> build $ words l)
  where build (n:b:s:_) = (n, stringToBoard b, stringToBoard s)
        build _ = undefined



main :: IO ()
main = do
  let gen = do
              bs <- genBoard
              bu <- genUnsolved bs
              return $ prop_solve bs bu
  quickCheck $ forAll gen (==True)
  hspec $ do
    ts <- runIO $ readTestcasesFromFile "test/testcases.txt"
    describe ("Testcases") $ do
      forM_ ts $ \(n, b, s) ->
                  it ("Check solution for testcase: " ++ n) $ do
                    (boardToString $ head $ solver b) `shouldBe` (boardToString s)


-- For debugging
traceBoard :: Board -> Board
traceBoard b = b
--traceBoard b = trace (boardToString b) b


