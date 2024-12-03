import System.IO

isSafeDiff :: Int -> Int -> Bool
isSafeDiff a b = do
  let diff = abs (a - b)
  diff >= 1 && diff <= 3

areMonotonicallyIncreasingOrDecreasing :: [Int] -> Bool
areMonotonicallyIncreasingOrDecreasing [] = True
areMonotonicallyIncreasingOrDecreasing [x] = True
areMonotonicallyIncreasingOrDecreasing [x, y] = x < y || x > y
areMonotonicallyIncreasingOrDecreasing (x:y:z:xs) = (x < y && y < z) || (x > y && y > z) && areMonotonicallyIncreasingOrDecreasing (y:z:xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe [x] = True
isSafe [x, y] = if isSafeDiff x y then True else False
isSafe (x:y:z:xs) = do
  areMonotonicallyIncreasingOrDecreasing [x, y, z] && isSafeDiff x y && isSafe (y:z:xs)

isSafe2 xs = do
  any ((== True) . isSafe) (generateAllSublists xs)

generateAllSublists :: [Int] -> [[Int]]
generateAllSublists [] = []
generateAllSublists (x:xs) = xs : map (x:) (generateAllSublists xs)

main = do
  handle <- openFile "02/input.txt" ReadMode
  contents <- hGetContents handle

  let records = lines contents
  let sequences = map (map read . words) records :: [[Int]]

  let allResults = map isSafe sequences
  let part1 = length (filter (== True) allResults)
  print part1

  let allResults2 = map isSafe2 sequences
  let part2 = length (filter (== True) allResults2)
  print part2
