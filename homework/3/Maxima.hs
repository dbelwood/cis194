module Maxima where
  localMaxima :: [Int] -> [Int]
  localMaxima xs =
    concat (map compFn triplets)
    where
      triplets        = zip3 xs (drop 1 xs) (drop 2 xs)
      compFn (x,y,z) = if y > x && y > z then [y] else []
