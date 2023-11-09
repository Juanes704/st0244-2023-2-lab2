module Lib2 where

import Data.List (transpose)

type Player = Char
type Grid = [[Player]]

size :: Int
size = 3

empty :: Grid 
empty = replicate size (replicate size ' ')

valid :: Grid -> Int -> Player -> Bool
valid g i b = 0 <= i && i < (size^2) && concat g !! i == ' '

won :: Grid -> Player -> Bool
won g p = wins p g

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

transp :: [[a]] -> [[a]]
transp ([]:_) = []
transp x = (map head x) : transp (map tail x)

wins :: Player -> Grid -> Bool
wins p g = any (all (== p)) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transp g
    dias = [diag g, reverseDiag g]

diag :: [[a]] -> [a]
diag [] = []
diag ([]:_) = []
diag ((x:_):xs) = x : diag (map tail xs)

reverseDiag :: [[a]] -> [a]
reverseDiag [] = []
reverseDiag ([]:_) = []
reverseDiag (xs) = last (head xs) : reverseDiag (map tail (tail xs))
