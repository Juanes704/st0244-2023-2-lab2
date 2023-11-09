module Lib where

--Tests for Function "valid" by QuickCheck
type Grid = [[Char]]

size :: Int
size = 3

b :: Char
b = 'b'

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < (size^2) && concat g !! i == b
