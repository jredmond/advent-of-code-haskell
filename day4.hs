import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy.Char8 (pack)

part1 :: Int
part1 = findHashNumber startWith5Zeros "yzbqklnj"

findHashNumber :: (String -> Bool) -> String -> Int
findHashNumber f s = head $ filter (\n -> f $ md5s (s ++ show n)) [1..]

startWith5Zeros :: String -> Bool
startWith5Zeros = (== "00000") . take 5

md5s :: String -> String
md5s = show . md5 . pack

-- Part 2:
part2 :: Int
part2 = findHashNumber startWith6Zeros "yzbqklnj"

startWith6Zeros :: String -> Bool
startWith6Zeros = (== "000000") . take 6