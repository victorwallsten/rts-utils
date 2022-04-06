module Main (main) where

import Data.List (isSuffixOf, sort)
import Data.List.Extra (dropEnd)
import Data.Maybe (listToMaybe)
import Makefile (ccRecipe, makeFile, targetCS)
import Parser (localHeaders)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getArgs)

userDefinedRule :: (String, [String]) -> String
userDefinedRule (c, hs) = unlines [targetCS (dropEnd 2 c) (unwords hs), ccRecipe]

userDefinedCFilesOf :: [FilePath] -> [FilePath]
userDefinedCFilesOf = filter userDefined . filter (isSuffixOf ".c")
  where
    userDefined =
      not
        . flip
          elem
          [ "startup.c",
            "TinyTimber.c",
            "canTinyTimber.c",
            "sciTinyTimber.c"
          ]

listCurrentDir :: IO [FilePath]
listCurrentDir = getCurrentDirectory >>= listDirectory

main :: IO ()
main = do
  arg1 <- listToMaybe <$> getArgs
  fs <- sort . userDefinedCFilesOf <$> listCurrentDir
  ss <- mapM readFile fs
  let fhs = [(f, localHeaders s) | (f, s) <- zip fs ss]
      udrs = concatMap userDefinedRule fhs
      os = dropEnd 3 $ unlines $ map (("         $(DEBUGDIR)" <>) . (<> ".o \\") . dropEnd 2) fs
      mf = makeFile arg1 udrs os
  writeFile "Makefile" mf
