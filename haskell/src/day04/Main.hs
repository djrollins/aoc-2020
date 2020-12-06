{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()
import Text.Regex.Base (getAllTextSubmatches)
import Text.Regex.PCRE ((=~))

requiredKeys :: S.Set String
requiredKeys = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parse :: String -> M.Map String String
parse = M.fromList . map toPairs . splitOn " "
  where
    toPairs :: String -> (String, String)
    toPairs str =
      let key : value : _ = splitOn ":" str
       in (key, value)

validateHeight :: String -> Bool
validateHeight height =
  case getAllTextSubmatches (height =~ "([0-9]+)(cm|in)") :: [String] of
    [_, h, "cm"] -> validateBetween 150 193 (read h)
    [_, h, "in"] -> validateBetween 59 76 (read h)
    _ -> False

validateBetween :: Int -> Int -> Int -> Bool
validateBetween minVal maxVal val = val >= minVal && val <= maxVal

validPassport :: M.Map String String -> Bool
validPassport passports = and . M.elems $ M.mapWithKey validate passports
  where
    validate :: String -> String -> Bool
    validate "byr" year = validateBetween 1920 2002 (read year)
    validate "iyr" year = validateBetween 2010 2020 (read year)
    validate "eyr" year = validateBetween 2020 2030 (read year)
    validate "hgt" height = validateHeight height
    validate "hcl" hair = hair =~ "^#[a-f0-9]{6}$"
    validate "ecl" eyes = eyes =~ "^(amb|blu|brn|gry|grn|hzl|oth)$"
    validate "pid" pid = pid =~ "^[0-9]{9}$"
    validate "cid" _ = True
    validate _ _ = False

part1 :: [M.Map String String] -> Int
part1 = length . filter ((== requiredKeys) . S.intersection requiredKeys . M.keysSet)

part2 :: [M.Map String String] -> Int
part2 = length . filter validPassport . filter ((== requiredKeys) . S.intersection requiredKeys . M.keysSet)

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- readFile filePath
  let passports = map (parse . unwords) . splitOn [""] . lines $ content
  print . part1 $ passports
  print . part2 $ passports
