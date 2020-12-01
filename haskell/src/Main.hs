module Main where

import           System.Environment
import           System.Directory
import           System.FilePath
import           System.IO

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead []      = Nothing

type Cartesian a = [a] -> [[a]]

dubs :: Cartesian a
dubs xs = [ [x, y] | x <- xs, y <- xs ]

trips :: Cartesian a
trips xs = [ [x, y, z] | x <- xs, y <- xs, z <- xs ]

part1 :: Cartesian Int -> [Int] -> Maybe Int
part1 cartesian numbers = product <$> safeHead candidates
  where candidates = filter (((==) 2020) . sum) $ cartesian numbers

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename         <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- readFile filePath
  let numbers = read <$> lines content
  putStrLn . show $ part1 dubs numbers
  putStrLn . show $ part1 trips numbers
