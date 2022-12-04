import Data.List
import Data.List.Split

tuplify2 [x, y] = (x, y)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine = tuplify2 . map (tuplify2 . map read . splitOn "-") . splitOn ","

isSubset :: (Int, Int) -> (Int, Int) -> Bool
isSubset (s1, e1) (s2, e2) = s1 <= s2 && e1 >= e2

fOr2 :: Foldable t => t (p1 -> p2 -> Bool) -> p1 -> p2 -> Bool
fOr2 fs x y = any (($ y) . ($ x)) fs

containsFully :: (Int, Int) -> (Int, Int) -> Bool
containsFully = fOr2 [isSubset, flip isSubset]

isOverlapping :: (Int, Int) -> (Int, Int) -> Bool
isOverlapping (s1, e1) (s2, e2) = e1 >= s2 && s1 <= e2

main = do
  input <- map parseLine . lines <$> readFile "input.txt"
  print $ length . filter (uncurry containsFully) $ input
  print $ length . filter (uncurry isOverlapping) $ input