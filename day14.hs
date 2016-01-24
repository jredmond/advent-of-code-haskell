import Control.Applicative
import Data.List (transpose)

type Spec = (Int, Int, Int)
type Resting = Bool
type State = (Resting, Int, Int)
data Deer = Deer Spec State

parse :: [String] -> [Deer]
parse = map (f . words) 
  where f [_,_,_,speed,_,_,flyTime,_,_,_,_,_,_,restTime,_] 
         = Deer (read speed,read flyTime,read restTime) (True, 0, 0)

-- state either resting or not flying
simulate1Second :: Deer -> Deer
simulate1Second (Deer (s,f,r) (state,timeLeft,dist))
  | timeLeft == 0 && state = Deer (s,f,r) (not state, f - 1, dist + s)
  | timeLeft == 0          = Deer (s,f,r) (not state, r - 1, dist)
  | state                  = Deer (s,f,r) (state, timeLeft - 1, dist)
  | otherwise              = Deer (s,f,r) (state, timeLeft - 1, dist + s)

getDistance :: Deer -> Int
getDistance (Deer _ (_,_,dist)) = dist

input = lines <$> readFile "day14Input.txt"
part1 = maximum . map (getDistance . (!! 2503) . iterate simulate1Second) . parse <$> input >>= print

-- Part 2
toPoints :: [Deer] -> [Int]
toPoints ds = map (toPoints' . getDistance) ds
  where maxDist = maximum (map getDistance ds)
        toPoints' dist
          | dist == maxDist = 1
          | otherwise       = 0


part2 = maximum . map sum . transpose . take 2503 . map toPoints . tail . iterate (map simulate1Second) . parse <$> input >>= print
