module Main (main) where

import Data.List
import Data.List.Split
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()

part1 :: String -> Int
part1 = sum . map (length . nub . filter (/= '\n')) . splitOn "\n\n"

part2 :: String -> Int
part2 = sum . map (length . nub . foldr intersect ['a' .. 'z'] . lines) . splitOn "\n\n"

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- readFile filePath

  print $ part1 content
  print $ part2 content
