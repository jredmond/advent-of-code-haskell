import Control.Applicative
import Data.List (subsequences, minimumBy)
import Data.Ord (comparing)

-- Part 1
input :: IO [Int]
input = map read . lines <$> readFile "day17Input.txt"
part1 = length . filter ((==150) . sum) . subsequences <$> input >>= print

part2 = length . f . filter ((==150) . sum) . subsequences <$> input >>= print
  where f xs = filter ((== minimum (map length xs)) . length) xs