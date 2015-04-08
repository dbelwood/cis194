{-# LANGUAGE TypeSynonymInstances #-}

module Calc where
  import ExprT
  import Parser
  import StackVM

  class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

  instance Expr ExprT where
    lit x = ExprT.Lit x
    add x y = ExprT.Add x y
    mul x y = ExprT.Mul x y

  reify :: ExprT -> ExprT
  reify = id

  instance Expr Integer where
    lit x = x
    add x y = x + y
    mul x y = x * y

  instance Expr Bool where
    lit x = if x < 0 then False else True
    add x y = x || y
    mul x y = x && y

  newtype MinMax  =   MinMax Integer deriving (Eq, Show)
  instance Expr MinMax where
    lit x = MinMax x
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

  newtype Mod7    =   Mod7 Integer deriving (Eq, Show)
  instance Expr Mod7 where
    lit x = Mod7 x
    add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

  instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

  class Evaluable a where
    eval :: a -> Integer

  instance Evaluable ExprT where
    eval (ExprT.Lit x) = x
    eval (ExprT.Add x y) = (eval x) + (eval y)
    eval (ExprT.Mul x y) = (eval x) * (eval y)

  class Calculatable a where
    calculate :: a -> Maybe Integer

  instance (Evaluable m) => Calculatable (Maybe m) where
    calculate exp = case exp of
      Nothing -> Nothing
      Just x -> Just(eval x)

  evalStr :: String -> Maybe Integer
  evalStr formStr = calculate (
    Parser.parseExp ExprT.Lit ExprT.Add ExprT.Mul formStr)

  testExp :: Expr a => Maybe a
  testExp = parseExp lit add mul "(3 * -4) + 5"

  compile :: String -> Maybe Program
  compile str = parseExp lit add mul str
