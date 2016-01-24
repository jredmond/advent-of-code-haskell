-- Day 1: Not Quite Lisp
-- Directions:
-- "(" - up one floor
-- ")" - down one floor

import Data.List (elemIndex)

-- Part 1:
-- Usage:
-- part1 "day1Input.txt"

part1 fileName = do
    contents <- readFile fileName
    print (sumDirections contents)

sumDirections :: String -> Integer
sumDirections = foldr changeFloor 0

changeFloor :: Char -> Integer -> Integer
changeFloor '(' =  (+ 1)
changeFloor ')' =  subtract 1
changeFloor  _  =  id

-- Part 2:
part2 fileName = do
    contents <- readFile fileName
    print (findWhenBasement contents)

findWhenBasement :: String -> Maybe Int
findWhenBasement = elemIndex (-1) . scanl (flip changeFloor) 0

-- Could be cleaned up use scan for changing floor to improve code reuse