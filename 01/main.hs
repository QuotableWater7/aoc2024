import System.IO
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

diff :: Int -> Int -> Int
diff a b = abs (a - b)

sumDiff :: [Int] -> [Int] -> Int
sumDiff [] [] = 0
sumDiff [] _ = 0
sumDiff _ [] = 0
sumDiff (a:as) (b:bs) = diff a b + sumDiff as bs

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

sumSimilarity :: [Int] -> [Int] -> Int
sumSimilarity list1 list2 = do
  let occurrences = countOccurrences list2

  sum $ map (\x -> fromMaybe 0 (Map.lookup x occurrences) * x) list1


main = do
  handle <- openFile "01/input.txt" ReadMode
  contents <- hGetContents handle

  let records = lines contents
  let numberParts = map ((\[a, b] -> (read a::Int, read b::Int)) . words) records
  let left = sort $ map fst numberParts
  let right = sort $ map snd numberParts

  print $ sumDiff left right
  print $ sumSimilarity left right
