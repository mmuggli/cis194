{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
     deriving (Show)

instance HasVars VarExprT where
         var = Var

instance Expr VarExprT where
         lit = Lit
         add = Add
         mul = Mul
         
instance HasVars (M.Map String Integer -> Maybe Integer) where
         var s = (\m -> if M.member s m
                           then Just (m M.! s)
                           else Nothing)
         
instance Expr (M.Map String Integer -> Maybe Integer) where
         lit i = (\m -> Just i)
         add f g = (\m -> case f m of
             Nothing -> Nothing
             Just a -> case g m of
                  Nothing -> Nothing
                  Just b -> Just (a + b))
         mul f g = (\m -> case f m of
             Nothing -> Nothing
             Just a -> case g m of
                  Nothing -> Nothing
                  Just b -> Just (a * b))
         
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
