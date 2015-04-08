module Golf where
  skips :: [a] -> [[a]]
  skips xs = (map (\ x -> (takeEvery x xs)) [1..length(xs)])
    where takeEvery n ys
            | length(ys) >= n = 
              (drop ( n - 1) (take n ys)) ++ (takeEvery n (drop n ys))
            | otherwise = ys
