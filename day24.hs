import Control.Applicative
import Data.List (find)
import Data.Maybe (fromJust, isJust)


input = map (read :: String->Int) . lines <$> readFile "day24Input.txt"
part1 = minimum . quantumEntanglement . valid3Partions <$> input >>= print

-- Find smallest groups to sum to that weight
minSizePotentialPartitions :: Int -> [Int] -> Maybe [[Int]]
minSizePotentialPartitions n xs = 
    find ((>0) . length) . map (filter ((== partitionWeight n xs) . sum)) . reverse $ subsequencesBySize xs
    
partitionWeight n = (`div` n) . sum

subsequencesBySize [] = [[[]]]
subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                            in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

-- Given the set and the first partion try and find a second partion (therefore can partion into 3)
isValid3Partion :: [Int] -> [Int] -> Bool
isValid3Partion xs partion1 = isJust . minSizePotentialPartitions 2 . filter (`notElem` partion1) $ xs

valid3Partions :: [Int] -> [[Int]]
valid3Partions xs = filter (isValid3Partion xs) . fromJust $ minSizePotentialPartitions 3 xs

-- if multiple solutions found in that group size min quantum entanglement
quantumEntanglement = map product

-- Part 2
validNPartions :: Int -> [Int] -> [[Int]]
validNPartions n xs = filter (isValidNPartion (n-1) xs) . fromJust $ minSizePotentialPartitions n xs

isValidNPartion :: Int -> [Int] -> [Int] -> Bool
isValidNPartion 1 _ _         = True
isValidNPartion n xs partion1 = isJust . find ((>0) . length) . filter (isValidNPartion (n-1) remainder) . fromJust $ minSizePotentialPartitions n remainder
  where remainder = filter (`notElem` partion1) xs

part2 = minimum . quantumEntanglement . validNPartions 4 <$> input >>= print

-- part2 generalisation works but is obtuse