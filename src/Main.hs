module Main where

import Sudoku

main :: IO ()
main = do
  boards <- readBoardsFromFile "all.txt"
  let solvedBoards = (fmap . fmap) (head . solver) boards
  ints <- mapM (\(bNum, b) ->
                 do
                   putStrLn $ boardToString b
                   return $ getTopLeftNum b
               ) solvedBoards
  putStrLn $ "Sum of top left: " ++ (show $ sum ints)
  return ()

