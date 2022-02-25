module Main (main) where

import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
import Makefile (ccRecipe, makeFile, targetCS)
import Parser (localHeaders)
import System.Directory (getCurrentDirectory, listDirectory)
import System.Environment (getArgs)

userDefinedRule :: (String, [String]) -> String
userDefinedRule (c, hs) = unlines [targetCS (init $ init c) (unwords hs), ccRecipe]

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
  fs <- userDefinedCFilesOf <$> listCurrentDir
  ss <- mapM readFile fs
  let fhs = [(f, localHeaders s) | (f, s) <- zip fs ss]
      udrs = concatMap userDefinedRule fhs
      os = init $ init $ unlines $ map (("         $(DEBUGDIR)" <>) . (<> ".o \\") . init . init) fs
      mf = makeFile arg1 udrs os
  writeFile "Makefile" mf
