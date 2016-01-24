-- Day 2: I Was Told There Would Be No Math
import Data.List.Split (splitOn)
import Data.List (sort)

-- Part 1:
-- Usage:
-- part1 "day2Input.txt"
part1 :: String -> IO ()
part1 fileName = 
    readFile fileName >>= print . sumFile (wrappingPaper . dimensions)

sumFile :: (String -> Int) -> String -> Int
sumFile calcLine file = sum $ map calcLine (lines file)

wrappingPaper :: [Int] -> Int
wrappingPaper [a, b, c] = a * b + 2 * (a * b) + 2 * (b * c) + 2 * (a * c)

-- "23x53x12" -> [12,23,53]
dimensions :: String -> [Int]
dimensions str = sort $ map read $ splitOn "x" str

-- Part 2
ribbon :: [Int] -> Int
ribbon [a, b, c] = a*b*c + a + a + b + b

part2 :: String -> IO ()
part2 fileName = 
    readFile fileName >>= print . sumFile (ribbon . dimensions)
