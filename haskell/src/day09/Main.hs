module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()
import Data.List.Split

part1 :: [Int] -> Int
part1 numbers =
  let (first25, rest) = splitAt 25 numbers

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- lines <$> readFile filePath
  print content

