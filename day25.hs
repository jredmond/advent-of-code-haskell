
part1 = iterate nextValue 20151125 !! (codeIndex (2947,3029) - 1)

codeIndex :: (Int,Int) -> Int
codeIndex (r,c) = triangularNumber (c + rowDiff) - rowDiff
  where 
    triangularNumber n = (n * (n + 1)) `div` 2
    rowDiff = r - 1

nextValue :: Int -> Int
nextValue n = n * 252533 `rem` 33554393

-- you had codeIndex (r,c) returning the index in row c, column r
-- correct answer is 19980801 (looks like a date doesn't it...)
