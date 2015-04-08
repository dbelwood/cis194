{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

  x :: Stream Integer
  x = Cons 0 (Cons 1 (streamRepeat 0))

  instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate stream = streamMap (\x -> (-1) * x) stream
    (+) stream1 stream2 =
      addStreams stream1 stream2
        where addStreams (Cons x xs) (Cons y ys) = Cons (x + y) (addStreams xs ys)
    (*) stream1 stream2 =
      multStreams stream1 stream2
        where multStreams (Cons x xs) (Cons y ys) =
                Cons (x * y) ((secondCoef x ys) + (thirdCoef xs y ys))
              secondCoef a bs = streamMap (\b -> a * b) bs
              thirdCoef as b bs = multStreams as (Cons b bs)

  instance Fractional (Stream Integer) where
    (/) stream1 stream2 =
      divStreams stream1 stream2
        where divStreams (Cons x xs) (Cons y ys) =
                Cons (quot x y) (secondCoef y (thirdCoef xs (divStreams stream1 stream2 ) ys))
              secondCoef b cs = streamMap (\c -> ((quot 1 b) * c)) cs
              thirdCoef stream1 stream2 stream3 = stream1 + negate(stream2) * stream3

  fibs3 :: Stream Integer
  fibs3 = x / (1 - x - x^2)

  data Matrix = Matrix (Integer, Integer) (Integer, Integer) deriving (Show)

  instance Num Matrix where
    (*) (Matrix (a,b) (p,q)) (Matrix (c, d) (r, s)) =
      Matrix
        (((a * c) + (b * d)), ((a * d) + (b * s)))
        (((p * c) + (q * d)), ((p * d) + (q * s)))

  fib4 :: Integer -> Integer
  fib4 0 = 0
  fib4 n = unpackMatrix (fib4' n)
    where unpackMatrix (Matrix (a,b) (p,q)) = p

  fib4' :: Integer -> Matrix
  fib4' 1 = Matrix (1,1) (1,0)
  fib4' n = if (even n) then fib_even(n) else fib_odd(n)
    where
      fib_even (n)  = (fib4' (quot n 2)) ^ 2
      fib_odd (n)   = (fib4' 1) * (fib4' (quot (n - 1) 2) ^ 2)
