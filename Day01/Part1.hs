import Data.List
import Data.List.Split

parseInput :: String -> [Int]
parseInput =  map (sum . map read . lines) . splitOn "\n\n"

main = do
    input <- reverse . sort . parseInput <$> readFile "input.txt"
    print $ head input
    print $ sum $ take 3 input