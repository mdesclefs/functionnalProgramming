module Utils where

import Data.List

findElem :: Char -> [Char] -> Int
findElem pattern line = case elemIndex pattern line of
    Nothing -> -1
    Just index -> index