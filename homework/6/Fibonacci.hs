module Fibonacci where
  import Data.List

  fib :: Integer -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = (fib (n - 1)) + (fib (n - 2))

  fibs1 :: [Integer]
  fibs1 = map fib [0..]

  fibs2 :: [Integer]
  fibs2 = [0, 1] ++ (
    map
      (\ (x,y) -> sum[x,y])
      (iterate (\ (x, y) -> (y, y + x)) (0,1)))

  data Stream a = Cons a (Stream a)

  streamToList :: Stream a -> [a]
  streamToList (Cons x xs) = x:(streamToList xs)

  instance Show a => Show (Stream a) where
    show x = show (take 20 (streamToList x))

  streamRepeat :: a -> Stream a
  streamRepeat x = Cons x (streamRepeat x)

  streamMap :: (a -> b) -> Stream a -> Stream b
  streamMap f (Cons x (xs)) = Cons (f x) (streamMap f xs)

  streamFromSeed :: (a -> a) -> a -> Stream a
  streamFromSeed f x = Cons x (streamFromSeed f (f x))

  interleaveStreams :: Stream a -> Stream a -> Stream a
  interleaveStreams (Cons x xs) ys = (Cons x (interleaveStreams ys xs))

  nats :: Stream Integer
  nats = streamFromSeed (\x -> x + 1) 0

  ruler :: Stream Integer
  ruler = ruler' 0 1

  ruler' ::  Integer -> Integer -> Stream Integer
  ruler' x y =
    interleaveStreams
      (streamRepeat x)
      (ruler' y (y + 1))
