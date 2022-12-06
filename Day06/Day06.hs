import Data.List

windowed :: Int -> [a] -> [[a]]
windowed size ls =
  case ls of
    [] -> []
    x : xs ->
      if length ls >= size
        then take size ls : windowed size xs
        else windowed size xs

findStartOfPacket :: Eq a => Int -> [a] -> Maybe Int
findStartOfPacket size = fmap (+ size) . findIndex ((== size) . length . nub) . windowed size

main = do
  input <- readFile "input.txt"
  print $ findStartOfPacket 4 input
  print $ findStartOfPacket 14 input