import System.IO
import Text.Regex.TDFA ((=~))

extractMatches :: String -> [(Int, Int)]
extractMatches contents = map (\[_, x, y] -> (read x, read y)) matches
  where matches = contents =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]

main :: IO ()
main = do
    handle <- openFile "03/input.txt" ReadMode
    contents <- hGetContents handle

    let part1 = sum $ map (\(x, y) -> x * y) $ extractMatches contents
    print part1
