module Main (main) where

import Data.List (foldl', tails)
import Debug.Trace
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

part1 :: [Int] -> Int
part1 numbers =
  let (first25, rest) = splitAt 25 numbers
   in case foldl' findInvalid (Left first25) rest of
        (Right val) -> val
        _ -> error "no valid solution"
  where
    findInvalid (Right val) _ = Right val
    findInvalid (Left prev25) current = if isValid prev25 current then Left (tail prev25 ++ [current]) else Right current
    isValid prev25 current = current `elem` map (uncurry (+)) (pairs prev25)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- lines <$> readFile filePath
  let numbers = read <$> content :: [Int]
  let part1_result = part1 numbers
  print part1_result
  print $ part2 numbers part1_result

part2 :: [Int] -> Int -> Int
part2 numbers total =
  let (range, totals) = unzip $ takeWhile ((<= total) . snd) (zip numbers (scanl1 (+) numbers))
   in if last totals == total
        then minimum range + maximum range
        else part2 (tail numbers) total
