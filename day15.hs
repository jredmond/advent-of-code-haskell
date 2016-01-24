import Control.Applicative
import Data.List (transpose)

-- permutaions tests
cases = length [(a,b,c,d,a+b+c+d) | a <- [0..100], 
                                    b <- [0..100-a],
                                    c <- [0..100-a-b],
                                    let d = 100-a-b-c]


factorial n = product [1..n]

--test n = ((n+1)^3 + 3*(n+1)^2 +2*(n+1)) / factorial 3

f 1 _ = 1
f k 0 = 1
f k n = sum (map (f (k-1)) [0..n])

-- Part 1
data Ingredient = Ingredient { capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             } deriving (Show)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

parse :: [String] -> [Ingredient]
parse = map (f . words . stripChars ":,")
  where f [name,_,cap,_,dur,_,fla,_,tex,_,cal]
         = Ingredient (read cap) (read dur) (read fla) (read tex) (read cal)

-- generate all combinations of k integers 0..n that sum to n (integer partitions)
generate :: Int -> Int -> [[Int]]
generate 1 n = [[n]]
generate k n = [x:rest | x <- [0..n], rest <- generate (k-1) (n-x)]

toProperties :: Ingredient -> Int -> [Int]
toProperties i n = map (n*) [capacity i,durability i,flavor i,texture i]

--owl operator
-- (f .: g) x y = f (g x y)
infixr 8 .:
(.:) = (.).(.)

cookieScore :: [Ingredient] -> [Int] -> Int
cookieScore = product . map (max 0 . sum) . transpose .: zipWith toProperties

input = lines <$> readFile "day15Input.txt"
part1 = maximum . f . parse <$> input >>= print
  where
    f is = map (cookieScore is) $ generate (length is) 100

-- part 2
caloriesScore :: [Ingredient] -> [Int] -> Int
caloriesScore = sum .: zipWith (\i n -> n * calories i)

bothScores :: [Ingredient] -> [Int] -> (Int, Int)
bothScores is ns = (cookieScore is ns, caloriesScore is ns)

part2 = maximum . f . parse <$> input >>= print
  where
    f is = map fst . filter ((==500) . snd) . map (bothScores is) $ generate (length is) 100