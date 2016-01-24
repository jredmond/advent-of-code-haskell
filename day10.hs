import Data.List (group, concatMap)

part1 = length $ iterate nextSequence "1113122113" !! 40
part2 = length $ iterate nextSequence "1113122113" !! 50

nextSequence :: String -> String
nextSequence = concatMap encode . group
  where encode xs = show (length xs) ++ [head xs]