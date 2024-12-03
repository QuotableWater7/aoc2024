import System.IO

data Result = Safe | Unsafe deriving (Eq, Show)

isSafeDiff :: Int -> Int -> Bool
isSafeDiff a b = do
  let diff = abs (a - b)
  diff >= 1 && diff <= 3

areMonotonicallyIncreasingOrDecreasing :: [Int] -> Bool
areMonotonicallyIncreasingOrDecreasing [] = True
areMonotonicallyIncreasingOrDecreasing [x] = True
areMonotonicallyIncreasingOrDecreasing [x, y] = x < y || x > y
areMonotonicallyIncreasingOrDecreasing (x:y:z:xs) = (x < y && y < z) || (x > y && y > z) && areMonotonicallyIncreasingOrDecreasing (y:z:xs)

getResult :: [Int] -> Result
getResult [] = Safe
getResult [x] = Safe
getResult [x, y] = if isSafeDiff x y then Safe else Unsafe
getResult (x:y:z:xs) = do
  if areMonotonicallyIncreasingOrDecreasing [x, y, z] && isSafeDiff x y then getResult (y:z:xs) else Unsafe


main = do
  handle <- openFile "02/input.txt" ReadMode
  contents <- hGetContents handle

  let records = lines contents
  let sequences = map (map read . words) records :: [[Int]]

  let allResults = map getResult sequences
  let part1 = length (filter (== Safe) allResults)
  print part1
