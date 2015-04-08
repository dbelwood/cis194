module TreeFold where
  data Tree a = Leaf |
                Node Integer Tree(a) a Tree(a)
    deriving(Show, Eq)

  foldTree :: [a] -> Tree a
  foldTree xs = foldr buildTree xs
    where buildTree f s =
            Node 0 Leaf a Leaf
