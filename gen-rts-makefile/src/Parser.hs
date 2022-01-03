module Parser (localHeaders) where

import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitWhen)

localHeaders :: String -> [String]
localHeaders =
  map (filter (/= '"'))
    . filter ((&&) . isPrefixOf "\"" <*> isSuffixOf "\"")
    . concatMap (take 1)
    . drop 1
    . splitWhen (== "#include")
    . words
