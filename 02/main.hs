import System.IO

isSafeDiff :: Int -> Bool
isSafeDiff x = x >= 1 && x <= 3

allSameSign :: [Int] -> Bool
allSameSign [] = True
allSameSign (x:xs) = all (== signum x) $ map signum xs

isSafe :: [Int] -> Bool
isSafe xs = all (isSafeDiff . abs) deltas && allSameSign deltas
  where deltas = zipWith (-) (tail xs) xs

generateAllSublists :: [Int] -> [[Int]]
generateAllSublists [] = []
generateAllSublists (x:xs) = xs : map (x:) (generateAllSublists xs)

isSafe2 :: [Int] -> Bool
isSafe2 = any isSafe . generateAllSublists

main = do
  handle <- openFile "02/input.txt" ReadMode
  contents <- hGetContents handle

  let records = lines contents
  let sequences = map (map read . words) records :: [[Int]]

  let part1 = length $ filter (== True) $ map isSafe sequences
  print part1

  let allResults2 = length $ filter (== True) $ map isSafe2 sequences
  print allResults2
