type Peg  = String
type Move = (Peg, Peg)

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 1 x y _ = [(x,y)]
hanoi3 n x y z = (hanoi3 (n - 1) x z y) ++ (hanoi3 1 x y z) ++ (hanoi3 (n - 1) z y x)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 2 a b c d = (hanoi4 1 a d b c) ++ (hanoi4 1 a b c d) ++ (hanoi4 1 d b a c)
hanoi4 3 a b c d = (hanoi4 1 a d b c) ++ (hanoi4 1 a c b d) ++ (hanoi4 1 a b c d) ++ (hanoi4 1 c b a d) ++ (hanoi4 1 d b a c)
hanoi4 4 a b c d = (hanoi4 1 a d b c) ++ (hanoi4 1 a c b d) ++ (hanoi4 1 a d b d) ++ (hanoi4 1 a b c d) ++ (hanoi4 1 d b a c) ++ (hanoi4 1 c b a d) ++ (hanoi4 1 c b a d) ++ (hanoi4 1 d b a c) ++ (hanoi4 1 c b a d)
-- hanoi4 n w x y z = (hanoi4 (n - 2) w z x y) ++ (hanoi4 (n - 3) w x y z) ++ (hanoi4 (n - 1) z x w y)

test :: IO ()
test = do
  print ([("a", "b")] == (hanoi3 1 "a" "b" "c"))
  print ([("a", "c"), ("a", "b"), ("c", "b")] == (hanoi3 2  "a" "b" "c"))
  print ([("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")] == (hanoi3 3 "a" "b" "c"))
  print ([("a", "c"), ("a", "b"), ("c", "b"), ("a", "c"), ("b", "a"), ("b", "c"), ("a", "c"), ("a", "b"), ("c", "b"), ("c", "a"), ("b", "a"), ("c", "b"), ("a", "c"), ("a", "b"), ("c", "b")] == (hanoi3 4 "a" "b" "c"))
  print ([("a", "b")] == (hanoi4 1 "a" "b" "c" "d"))
  print ([("a", "d"), ("a", "b"), ("d", "b")] == (hanoi4 2 "a" "b" "c" "d"))
  print ([("a", "d"), ("a", "c"), ("a", "b"), ("c", "b"), ("d", "b")] == (hanoi4 3 "a" "b" "c" "d"))
  print ([("a", "d"), ("a", "c"), ("a", "d"), ("a", "b"), ("d", "b"), ("c", "b"), ("c", "b"), ("d", "b"), ("c", "b")])
  print (hanoi4 4 "a" "b" "c" "d")
  -- print ([("a", "b"), ("a", "c"), ("a", "d"), ("b", "c"), ("a", "b"), ("d", "b"), ("c", "d"), ("c", "b"), ("d", "b")] == (hanoi4 4 "a" "b" "c" "d"))
 
-- Move 1 disc from a -> b where pegs are a b c d
--        [ [1] []  []  [] ]
-- a -> b [ []  [1] []  [] ]
--
-- Move 2 discs from a -> b where pegs are a b c d
--        [ [1:2] []      []    []  ]
-- a -> d [ [2]   []      []    [1] ]
-- a -> b [ []    [2]     []    [1] ]
-- d -> b [ []    [1:2]   []    [] ]
--
-- Move 3 discs from a -> b where pegs are a b c d
--        [ [1:2:3] []      []    []  ]
-- a -> c [ [2:3]   []      [1]   []  ]
-- a -> d [ [3]     []      [1]   [2] ]
-- a -> b [ []      [3]     [1]   [2] ]
-- d -> b [ []      [2:3]   [1]   [] ]
-- c -> b [ []      [1:2:3] []    [] ]
--
-- Move 4 discs from a -> b where pegs are a b c d
-- [("a","d"),("a","c"),("a","d"),("a","b"),("d","b"),("c","b"),("c","b")] 
--        [ [1:2:3:4] []        []    []    ]
-- a -> c [ [2:3:4]   []        []    [1]   ]
-- a -> d [ [3:4]     []        [2]   [1]   ]
-- a -> b [ [4]       [3]       [2]   [1]   ]
-- d -> b [ [4]       [3]       [1:2] []    ]
-- c -> b [ [4]       []        [1:2] [3]   ]
-- c -> b [ []        [4]       [1:2] [3]   ]
-- c -> b [ []        [3:4]     [1:2] []    ]
-- d -> b [ []        [3:4]     [2]   [1]   ]
-- c -> b [ []        [1:2:3:4] []    []    ]
--
