module Lib
    ( someFunc,
      neverDecreases 
    ) where

import Data.Char(digitToInt)
import System.Environment
import Data.List(groupBy)

neverDecreases :: Ord a => [a] -> Bool
neverDecreases [] = True
neverDecreases (x:[]) = True
neverDecreases (x:(y:xs)) = if y < x then False else neverDecreases (y:xs)

hasSameAdjacent :: Eq a => [a] -> Bool
hasSameAdjacent [] = False
hasSameAdjacent (x:[]) = False
hasSameAdjacent (x:(y:xs)) = if x == y then True else hasSameAdjacent (y:xs) 

hasOnlyTwoAdjacent :: Eq a => [a] -> Bool
hasOnlyTwoAdjacent ls = 
    let groups = groupBy (==) ls in 
    let lengths = map length groups in 
    any (==2) lengths

someFunc :: IO ()
someFunc = do
    let nums = map (map digitToInt) (map show [372037..905157])
    print (length (filter neverDecreases (filter hasOnlyTwoAdjacent nums)))
