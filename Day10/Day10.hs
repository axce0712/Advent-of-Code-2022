import Control.Monad
import Data.List
import Data.List.Split

parseLine :: String -> [Int]
parseLine line =
  case words line of
    ["noop"] -> [0]
    ["addx", x] -> [0, read x]
    _ -> error line

pixel :: Int -> Int -> String
pixel pos reg =
  if abs (mod pos 40 - reg) <= 1
    then "#"
    else "."

main = do
  cycles <- scanl (+) 1 . (parseLine <=< lines) <$> readFile "input.txt"
  print $ sum . map (\i -> i * cycles !! (i - 1)) $ [20, 60 .. 220]
  putStr
    $ unlines . map concat . chunksOf 40 . map (\i -> pixel i (cycles !! i))
    $ [0 .. 239]