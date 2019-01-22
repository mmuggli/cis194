{-# LANGUAGE TypeSynonymInstances #-}
import ExprT
import Parser

    
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
                 
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
              Nothing -> Nothing
              Just e -> Just (eval e)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
          
instance Expr Integer where
    lit = id
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit a = if a <= 0 then False else True
    add a b = a || b
    mul a b = a && b


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)
                            
              
              
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7          

--compile :: String -> Maybe Program
           
-- Simply create an instance of the Expr type class for Program
-- instance Expr Program where
--     lit a = [PushI a]
--     add a b = a ++ b ++ [Add]
--     mul a b = a ++ b ++ [Mul]
