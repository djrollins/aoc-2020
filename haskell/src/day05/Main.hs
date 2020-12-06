{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO ()

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  filename <- head <$> getArgs
  let filePath = currentDirectory </> filename
  content <- readFile filePath
  print content
