toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0      = []
  | otherwise   = toDigits (div x 10) ++ [mod x 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0      = []
  | otherwise   = (mod x 10) : toDigitsRev(div x 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []        = []
doubleEveryOther [x]       = [x]
doubleEveryOther (x:y:xs)  = (x:(2*y):(doubleEveryOther xs))

sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:xs)  = (div x 10) + (mod x 10) + sumDigits(xs)

validateCreditCard :: Integer -> Bool
validateCreditCard n
  | n <= 0      = False
  | otherwise   = (mod (sumDigits (doubleEveryOther (toDigitsRev (n)))) 10) == 0

test :: IO ()
test = do
  print (validateCreditCard 4012888888881881))
  print (validateCreditCard 4012888888881882))
