module Main where

import Sudoku

import Test.Hspec
import System.IO
import Control.Monad

-- (Name, Problem Board, Solution Board)
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
main = hspec $ parallel $ do
  ts <- runIO $ readTestcasesFromFile "test/testcases.txt"
  describe ("Testcases") $ do
    forM_ ts $ \(n, b, s) ->
                it ("Check solution for testcase: " ++ n) $ do
                  (boardToString $ head $ solver b) `shouldBe` (boardToString s)

