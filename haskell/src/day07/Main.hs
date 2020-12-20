module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()
import Text.ParserCombinators.Parsec hiding (count)

data BagType = BagType
  { material :: String,
    colour :: String
  }
  deriving (Show, Eq, Ord)

type Bag = (BagType, M.Map BagType Int)

parseBagType :: Parser BagType
parseBagType = do
  _material <- many letter <* char ' '
  _colour <- many letter
  return $ BagType _material _colour

parseContents :: Parser (M.Map BagType Int)
parseContents = option M.empty $ fmap M.fromList (sepBy1 parseContainedBag (string ", ") <* char '.')
  where
    parseContainedBag = do
      count <- read <$> many1 digit <* char ' '
      bagT <- parseBagType <* string " bag" <* optional (char 's')
      return (bagT, count)

parseBag :: Parser Bag
parseBag = (,) <$> parseBagType <* string " bags contain " <*> parseContents

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- lines <$> readFile filePath
  let parsed = M.fromList <$> traverse (parse parseBag "") content
  print $ part1 <$> parsed
  print $ part2 <$> parsed

part1 :: M.Map BagType (M.Map BagType Int) -> Int
part1 bags = S.size $ countValidBags (BagType "shiny" "gold") allBags
  where
    allBags :: M.Map BagType [BagType]
    allBags = M.keys <$> bags
    countValidBags :: BagType -> M.Map BagType [BagType] -> S.Set BagType
    countValidBags toFind = M.foldlWithKey' (\set bagType contents -> if toFind `elem` contents then S.union (S.insert bagType set) (countValidBags bagType allBags) else set) S.empty

part2 :: M.Map BagType (M.Map BagType Int) -> Int
part2 bags = countTotalBagsIn (BagType "shiny" "gold")
  where
    countTotalBagsIn bagType =
      case M.lookup bagType bags of
        (Just content) -> M.foldlWithKey' (\total bagT count -> total + count + count * countTotalBagsIn bagT) 0 content
        Nothing -> 0
