module Main (main) where

import Data.Foldable (foldl')
import Data.List (sort)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()

part1 :: [Integer] -> Integer
part1 numbers = uncurry (*) . foldl' countDiffs (0, 1) $ zip (0 : numbers) numbers
  where
    countDiffs (ones, threes) (x, y)
      | (y - x) == 1 = (ones + 1, threes)
      | (y - x) == 3 = (ones, threes + 1)
      | otherwise = (ones, threes)

part2 :: [Integer] -> Integer
part2 numbers =
  let differences = zipWith (-) numbers (0 : numbers)
   in arrangements differences
  where
    arrangements diffs = case diffs of
      1 : 1 : 1 : 1 : xs -> arrangements xs * 7
      1 : 1 : 1 : xs -> arrangements xs * 4
      1 : 1 : xs -> arrangements xs * 2
      _ : xs -> arrangements xs
      [] -> 1

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- lines <$> readFile filePath
  let numbers = read <$> content :: [Integer]
  print $ part1 (sort numbers)
  print $ part2 (sort numbers)
