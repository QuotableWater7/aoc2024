import System.IO
import Text.Regex.TDFA ((=~))

extractMatches :: String -> [(Int, Int)]
extractMatches contents = map (\[_, x, y] -> (read x, read y)) matches
  where matches = contents =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]

removeBadStuff :: [[String]] -> [[String]]
removeBadStuff = go True
  where go _ [] = []
        go _ (["don't()", _, _]:xs) = go False xs
        go _ (["do()", _, _]:xs) = go True xs
        go True (x:xs) = x : go True xs
        go False (_:xs) = go False xs

main :: IO ()
main = do
    handle <- openFile "03/input.txt" ReadMode
    contents <- hGetContents handle

    let part1 = sum $ map (uncurry (*)) $ extractMatches contents
    print part1

    let sections = contents =~ "mul\\(([0-9]+),([0-9]+)\\)|don't\\(\\)|do\\(\\)" :: [[String]]
    let goodParts = removeBadStuff sections
    let parsedNumbers = map (\[_, x, y] -> (read x, read y)) goodParts :: [(Int, Int)]
    let resultPart2 = sum $ map (uncurry (*)) parsedNumbers
    print resultPart2
