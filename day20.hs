import Data.List (findIndex)
import qualified Data.Set as Set

--part 1
main = do
    print . findIndex (>= 33100000) . map presents1 $ [0..]
    print . findIndex (>= 33100000) . map presents2 $ [0..]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

factors :: Int -> [Int]
factors n = ordNub . concat $ [[x, q] | x <- [1..isqrt(n)], let (q, r) = divMod n x, r == 0]

presents1 :: Int -> Int
presents1 = (10 *) . sum . factors

presents2 :: Int -> Int
presents2 n = (11 *) . sum . filter (> (n-1) `div` 50) . factors $ n

-- O(n log n) rather than nub's O(n²)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

presentsSlow :: Int -> [Int]
presentsSlow i = (10*i) : zipWith (+) (tail . cycle $ (10*i) : replicate (i-1) 0) (presentsSlow (i+1))

factorsNaive n = [i | i <- [1..n], (mod n i) == 0]