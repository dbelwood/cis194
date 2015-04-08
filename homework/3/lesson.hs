data IntList = Empty | Cons Int IntList
               deriving(Show)

-- Naive Map implementation
mapIntList :: IntList -> (Int -> Int) -> IntList
mapIntList Empty _        = Empty
mapIntList (Cons x xs) fn = (Cons (fn x)) (mapIntList xs fn)

-- Naive Filter implementation
filterIntList :: IntList -> (Int -> Bool) -> IntList
filterIntList Empty _ = Empty
filterIntList (Cons x xs) fn
  | (fn x)    = Cons x (filterIntList xs fn)
  | otherwise = filterIntList xs fn

-- Naive Fold implementation
foldIntList :: IntList -> (Int -> Int -> Int) -> Int -> Int
foldIntList Empty fn start = start
foldIntList (Cons x xs) fn start = foldIntList xs fn (fn start x)

data List t = E | C t (List t)
              deriving(Show)

-- filterList :: (List t) -> (t -> Bool) -> (List t)
filterList E _ = E
filterList (C x xs) fn
  | (fn x)    = C x (filterList xs fn)
  | otherwise = filterList xs fn

mapList :: List a -> (a -> b) -> List b
mapList E _ = E
mapList (C x xs) fn = C (fn x) (mapList xs fn)
