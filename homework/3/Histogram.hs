module Histogram where
  import Data.List
  import Data.Char

  histogram :: [Integer] -> String
  histogram xs = unlines (
      histoString ++ 
      ["=========="]
      ["0123456789"]
      )
    where 
      range       = [0..9]
      histo       = map (\x -> length(elemIndices x xs)) range
      cols        = map 
        (\s -> (replicate s '*') ++ (replicate ((maximum histo) - s) ' ')) 
        histo
      rowFn m     = concat (map (\n -> take 1 (drop m n)) cols)
      histoString = (reverse (map rowFn [0..((maximum histo) - 1)]))
