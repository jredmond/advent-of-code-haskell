import Data.List (foldl')
import qualified Data.Map.Strict as M

type Move = Char
type Position = (Int, Int)
type State = (Position, M.Map Position Int)

-- Part 1:
-- Usage: part1 "day3Input.txt"
part1 :: String -> IO ()
part1 fileName = readFile fileName >>= print . M.size . recordMoves

-- foldl' :: (b -> a -> b) -> b -> [a] -> b
recordMoves :: [Move] -> M.Map Position Int
recordMoves = snd . foldl' recordMove initState
  where 
    initState = ((0,0), M.singleton (0,0) 1)

recordMove :: State -> Move -> State
recordMove (p, countMap) m = (p', M.insertWith (+) p' 1 countMap)
  where p' = move p m 

move :: Position -> Move -> Position
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)

-- Part 2:
-- Usage: part2 "day3Input.txt"
-- (SantaOrRobo, santaPos, roboPos, countMap)
type State2 = (Bool, Position, Position, M.Map Position Int)

part2 :: String -> IO ()
part2 fileName = readFile fileName >>= print . M.size . recordMoves2

recordMoves2 :: [Move] -> M.Map Position Int
recordMoves2 = getMap . foldl' recordMove2 initState
  where 
    initState = (True, (0,0), (0,0), M.singleton (0,0) 2)

recordMove2 :: State2 -> Move -> State2
recordMove2 (b, s, r, countMap) m
  | b         = (not b, s', r, add s')
  | otherwise = (not b, s, r', add r')
    where s' = move s m
          r' = move r m
          add p = M.insertWith (+) p 1 countMap

getMap :: State2 -> M.Map Position Int
getMap (_,_,_,m) = m