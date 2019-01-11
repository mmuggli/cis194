{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Parser
import StackVM

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr Integer where
    lit = id
    add a b = a + b
    mul a b = a * b

           
instance Expr Program  where
    lit a = [PushI a]
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
              
compile :: String -> Maybe Program
compile s = parseExp lit add mul s

run :: Maybe Program -> Either String StackVal
run Nothing = Left "Bad expression probably"
run (Just p) = stackVM p
